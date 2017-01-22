package xyz.hyperreal.cras

import java.sql.Statement

import collection.mutable.ListBuffer


object CommandFunctionHelpers {
	def insertCommand( env: Env, resource: Table, json: OBJ ) = {
		val com = new StringBuilder( "INSERT INTO " )
		val json1 = escapeQuotes( json )

		com ++= resource.name
		com ++= resource.names.
			filterNot (n => resource.columns(env.db.desensitize( n )).typ.isInstanceOf[ManyReferenceType]).
			mkString( " (", ", ", ") " )
		com ++= "VALUES ("

		val values = new ListBuffer[String]

		for (c <- resource.names)
			json1 get c match {
				case None if resource.columns(env.db.desensitize( c )).typ.isInstanceOf[ManyReferenceType] =>	// quietly ignored - not considered an error
				case None => values += "NULL"
				case Some( v ) =>
					resource.columns(env.db.desensitize( c )).typ match {
						case SingleReferenceType( tname, tref ) if v != null && !v.isInstanceOf[Int] && !v.isInstanceOf[Long] =>
							values += s"(SELECT id FROM $tname WHERE " +
								(tref.columns.values.find( c => c.unique ) match {
									case None => throw new CrasErrorException( "insert: no unique column in referenced resource in POST request" )
									case Some( uc ) => uc.name
								}) + " = '" + String.valueOf( v ) + "')"
						case ManyReferenceType( _, _ ) =>
							if (v eq null)
								throw new CrasErrorException( s"insert: manay-to-many field cannot be NULL: $c" )
						case StringType => values += '\'' + String.valueOf( v ) + '\''
						case _ => values += String.valueOf( v )
					}
			}

		com ++= values mkString ", "
		com += ')'
		com.toString
	}

	def uniqueColumn( resource: Table ) =
		resource.columns.values.find( c => c.unique ) match {
			case None => throw new CrasErrorException( s"insert: no unique column in '${resource.name}'" )
			case Some( uc ) => uc.name
		}
}

object CommandFunctions {
	def command( env: Env, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Env, resource: Table, id: Long ) = command( env, s"DELETE FROM ${resource.name} WHERE id = $id;" )
	
	def batchInsert( env: Env, resource: Table, rows: List[List[AnyRef]] ) {
		val types = for ((n, i) <- resource.names zipWithIndex) yield (i + 1, resource.columns(env.db.desensitize(n)).typ)
			
		for (r <- rows) {
			for (((i, t), c) <- types zip r) {
				(t, c) match {
					case (StringType, a: String) => resource.preparedInsert.setString( i, a )
					case (IntegerType, a: java.lang.Integer) => resource.preparedInsert.setInt( i, a )
					case (LongType, a: java.lang.Long) => resource.preparedInsert.setLong( i, a )
					case _ => sys.error( s"missing support for '$t'" )
				}
			}
				
			resource.preparedInsert.addBatch
		}
		
		resource.preparedInsert.executeBatch
		resource.preparedInsert.clearParameters
	}
	
	def insert( env: Env, resource: Table, json: OBJ ) = {
// 		if (json.keySet == resource.names.toSet) {
// 			
// 			for ((n, i) <- resource.names zipWithIndex)
// 				json(n) match {
// 					case a: Integer => resource.preparedInsert.setInt( i + 1, a )
// 					case a: String => resource.preparedInsert.setString( i + 1, a )
// 				}
// 				
// 			resource.preparedInsert.executeUpdate
// 			resource.preparedInsert.clearParameters
// 			
// 			val g = resource.preparedInsert.getGeneratedKeys
// 			
// 			g.next
// 			g.getLong(1)
// 		} else {
		val com = CommandFunctionHelpers.insertCommand( env, resource, json )
		val id =
			if (env.db == PostgresDatabase) {
				val res = env.statement.executeQuery( com + "RETURNING id" )

				res.next
				res.getLong( 1 )
			} else {
				env.statement.executeUpdate( com, Statement.RETURN_GENERATED_KEYS )

				val g = env.statement.getGeneratedKeys

				g.next
				g.getLong( 1 )
			}

		val values = new ListBuffer[(String, String)]

		resource.columns.values.foreach {
			case Column( col, ManyReferenceType(tab, ref), _, _, _, _ ) =>
				json get col match {
					case None =>
					case Some( vs ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							values += (s"(SELECT id FROM $tab WHERE " +
								CommandFunctionHelpers.uniqueColumn( ref ) + " = '" + String.valueOf( v ) + "')" -> tab)
				}
			case _ =>
		}

		if (values nonEmpty) {
			values groupBy {case (_, r) => r} foreach {
				case (tab, vs) =>
					for ((v, _) <- vs)
						command( env, s"INSERT INTO ${resource.name}$$$tab VALUES ($id, $v)" )
			}
		}

		id
	}
	
	def update( env: Env, resource: Table, id: Long, json: OBJ, all: Boolean ) =
		if (all && json.keySet != resource.names.toSet)
			throw new CrasErrorException( "update: missing column(s) in PUT request" )
		else {
			val com = new StringBuilder( "UPDATE " )
			var typ: ColumnType = null
			
			com ++= resource.name
			com ++= " SET "
			com ++=
				escapeQuotes( json ).toList map {
					case (k, v) if resource.columns(env.db.desensitize(k)).typ == StringType => k + " = '" + String.valueOf( v ) + "'"
					case (k, v) if {typ = resource.columns(env.db.desensitize(k)).typ; typ.isInstanceOf[SingleReferenceType]} =>
						if (v.isInstanceOf[Int] || v.isInstanceOf[Long])
							k + " = " + String.valueOf( v )
						else {
							val reft = typ.asInstanceOf[SingleReferenceType].ref
							val refc =
								reft.columns.values.find( c => c.unique ) match {
									case None => throw new CrasErrorException( "update: no unique column in referenced resource in PUT/PATCH request" )
									case Some( c ) => c.name
								}

							k + s" = (SELECT id FROM $reft WHERE $refc = '$v')"
						}
					case (k, v) => k + " = " + String.valueOf( v )
				} mkString ", "
			com ++= " WHERE id = "
			com ++= id.toString
			env.statement.executeUpdate( com.toString )
		}

	def insertLinks( env: Env, resource: Table, id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new CrasErrorException( s"append: field not found: $field" )
			case Some( vs ) =>
				resource.columns( env.db.desensitize(field) ).typ match {
					case ManyReferenceType( _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							associateID( env, resource, id, ref, CommandFunctionHelpers.uniqueColumn(ref), v )
					case _ => throw new CrasErrorException( s"append: field not many-to-many: $field" )
				}
		}

	def deleteLinks( env: Env, resource: Table, id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new CrasErrorException( s"append: field not found: $field" )
			case Some( vs ) =>
				resource.columns( env.db.desensitize(field) ).typ match {
					case ManyReferenceType( _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							deleteLinkID( env, resource, id, ref, CommandFunctionHelpers.uniqueColumn( ref ), v )
					case _ => throw new CrasErrorException( s"append: field not many-to-many: $field" )
				}
		}

	def deleteLinksID( env: Env, resource: Table, id: Long, field: String, tid: Long ) =
		resource.columns( env.db.desensitize(field) ).typ match {
			case ManyReferenceType( _, ref ) => deleteLinkIDs( env, resource, id, ref, tid )
			case _ => throw new CrasErrorException( s"append: field not many-to-many: $field" )
		}

	def deleteLinkID( env: Env, src: Table, id: Long, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val dobj = QueryFunctions.findOne( env, dst, dfield, dvalue )( "id" ).asInstanceOf[Long]

		deleteLinkIDs( env, src, id, dst, dobj )
	}

	def deleteLinkIDs( env: Env, src: Table, sid: Long, dst: Table, did: Long ) =
		command( env, s"DELETE FROM ${src.name}$$${dst.name} WHERE ${src.name}$$id = $sid AND ${dst.name}$$id = $did" )

	def associateID( env: Env, src: Table, id: Long, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val dobj = QueryFunctions.findOne( env, dst, dfield, dvalue )( "id" )

		command( env, s"INSERT INTO ${src.name}$$${dst.name} VALUES ($id, $dobj)" )
	}

	def associate( env: Env, src: Table, sfield: String, svalue: AnyRef, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val sobj = QueryFunctions.findOne( env, src, sfield, svalue )( "id" ).asInstanceOf[Long]

		associateID( env, src, sobj, dst, dfield, dvalue )
	}
}
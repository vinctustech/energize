package xyz.hyperreal.energize

import java.sql.Types
import javax.sql.rowset.serial.SerialBlob


object CommandFunctionHelpers {

	def uniqueColumn( resource: Table ) =
		resource.columns find (_.unique ) match {
			case None => throw new BadRequestException( s"insert: no unique column in '${resource.name}'" )
			case Some( uc ) => uc.name
		}

}

object CommandFunctions {

	def command( env: Environment, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Environment, resource: Table, id: Long ) = command( env, s"DELETE FROM ${resource.name} WHERE id = $id;" )

	def deleteValue( env: Environment, resource: Table, field: String, value: Any ) =
		value match {
			case s: String => command( env, s"DELETE FROM ${resource.name} WHERE $field = '$s';" )
			case _ => command( env, s"DELETE FROM ${resource.name} WHERE $field = $value;" )
		}

	def batchInsert( env: Environment, resource: Table, rows: List[List[AnyRef]] ) {
		val types = for ((c, i) <- resource.columns zipWithIndex) yield (i + 1, c.typ)
			
		for (r <- rows) {
			for (((i, t), v) <- types zip r) {
				(t, v) match {
					case (StringType, a: String) => resource.preparedInsert.setString( i, a )
					case (IntegerType, a: java.lang.Integer) => resource.preparedInsert.setInt( i, a )
					case (LongType, a: java.lang.Long) => resource.preparedInsert.setLong( i, a )
					case _ => throw new BadRequestException( s"missing support for '$t'" )
				}
			}
				
			resource.preparedInsert.addBatch
		}
		
		resource.preparedInsert.executeBatch
		resource.preparedInsert.clearParameters
	}
	
	def insert( env: Environment, resource: Table, json: OBJ ): Long = {
		val json1 = escapeQuotes( json )
		val cols = resource.columns filterNot (c => c.typ.isInstanceOf[ManyReferenceType])
		val diff = json.keySet -- (cols map (_.name) toSet)

		if (diff nonEmpty)
			throw new BadRequestException( "insert: excess field(s): " + diff.mkString(", ") )

		val mtms = resource.columns filter (c => c.typ.isInstanceOf[ManyReferenceType])	// todo: mtm fields cannot be null; still needs to be checked

		for ((c, i) <- cols zipWithIndex) {
			def setNull: Unit = {
				val t =
					c.typ match {
						case BooleanType => Types.BOOLEAN
						case StringType => Types.VARCHAR
						case IntegerType => Types.INTEGER
						case FloatType => Types.FLOAT
						case LongType|SingleReferenceType( _, _ )|MediaType( _, _, _ ) => Types.BIGINT
						case BLOBType( _ ) => Types.BLOB
						case TimestampType => Types.TIMESTAMP
					}

				resource.preparedInsert.setNull( i + 1, t )
			}

			json1 get c.name match {
				case None => setNull
				case Some( null ) => setNull
				case Some( v ) =>
					c.typ match {
						case SingleReferenceType( _, tref ) if !v.isInstanceOf[Int] && !v.isInstanceOf[Long] =>
							resource.preparedInsert.setLong( i + 1, QueryFunctions.findOne(env, tref, CommandFunctionHelpers.uniqueColumn(tref), v).asInstanceOf[Map[String, Long]]("id") )
//								s"SELECT id FROM $tname WHERE " +
//								(tref.columns.find(c => c.unique ) match {
//									case None => throw new BadRequestException( "insert: no unique column in referenced resource in POST request" )
//									case Some( uc ) => uc.name
//								}) + " = '" + String.valueOf( v )

//						case ManyReferenceType( _, _ ) =>
//							if (v eq null)
//								throw new BadRequestException( s"insert: manay-to-many field cannot be NULL: $c" )
						case SingleReferenceType( _, _ ) => resource.preparedInsert.setLong( i + 1, v.asInstanceOf[Number].longValue )
						case BooleanType => resource.preparedInsert.setBoolean( i + 1, v.asInstanceOf[Boolean] )
						case IntegerType => resource.preparedInsert.setInt( i + 1, v.asInstanceOf[Int] )
						case FloatType => resource.preparedInsert.setDouble( i + 1, v.asInstanceOf[Double] )
						case LongType => resource.preparedInsert.setLong( i + 1, v.asInstanceOf[Long] )
						case TimeType | DateType | BinaryType | StringType | EnumType(_) => resource.preparedInsert.setString( i + 1, v.toString )
						case DatetimeType | TimestampType => resource.preparedInsert.setTimestamp( i + 1, env.db.readTimestamp(v.toString) )
						case ArrayType( _, dpos, dim, dimint ) => resource.preparedInsert.setObject( i + 1, v.asInstanceOf[Seq[Any]].toArray )
						case BLOBType( rep ) =>
							val array =
								rep match {
									case 'base64 => base642bytes( v.toString )
									case 'hex => v.toString grouped 2 map (s => Integer.valueOf(s, 16) toByte) toArray
									case 'array => Array( v.asInstanceOf[Seq[Int]].map(a => a.toByte): _* )
								}

							resource.preparedInsert.setBinaryStream( i + 1, new SerialBlob(array).getBinaryStream )
						case MediaType( allowed, _, limit ) =>
							val obj = v.asInstanceOf[OBJ]

							obj( "type" ).asInstanceOf[String] match {
								case t@Energize.MIME( typ, subtype ) =>
									if (allowed != Nil && !allowed.exists {case MimeType(atyp, asubtype) => typ == atyp && asubtype == "*" || subtype == asubtype})
										throw new BadRequestException( s"insert: MIME type not allowed: $t" )
								case t => throw new BadRequestException( s"insert: invalid MIME type: $t" )
							}

							val id = insert( env, env table "_media_", obj )

							resource.preparedInsert.setLong( i + 1, id )

						//						case _ => values += String.valueOf( v )
					}
			}
		}

		val id =
//			if (env.db == PostgresDatabase) {
//				val res = env.statement.executeQuery( com + "RETURNING id" )
//
//				res.next
//				res.getLong( 1 )
//			} else {
			{
				resource.preparedInsert.executeUpdate
//( Statement.RETURN_GENERATED_KEYS )
				val g = resource.preparedInsert.getGeneratedKeys

				g.next

				val res = g.getLong( 1 )

				resource.preparedInsert.clearParameters
				res
			}

		/* todo: this code allows values to be inserted into junction table for mtm columns
		val values = new ListBuffer[(String, String)]

		resource.columns.foreach {
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
*/

		id
	}
	
	def update( env: Environment, resource: Table, id: Long, json: OBJ, all: Boolean ) = {
		val fields = resource.columns filterNot (c => c.typ.isInstanceOf[ManyReferenceType]) map (c => c.name) toSet

		if (all && json.keySet != fields)
			if ((fields -- json.keySet) nonEmpty)
				throw new BadRequestException( "update: missing field(s): " + (fields -- json.keySet).mkString( ", " ) )
			else
				throw new BadRequestException( "update: excess field(s): " + (json.keySet -- fields).mkString( ", " ) )
		else {
			val com = new StringBuilder( "UPDATE " )
			//			var typ: ColumnType = null

			com ++= resource.name
			com ++= " SET "
			com ++=
				(for ((k, v) <- escapeQuotes( json ).toList)
					yield {
						resource.columnMap(k).typ match {
							case DatetimeType | TimestampType => k + " = '" + env.db.readTimestamp( v.toString ) + "'"
							case TimeType | DateType | StringType | EnumType(_) if v ne null => s"$k = '$v'"
							case ArrayType( _, _, _, _ ) => k + " = " + v.asInstanceOf[Seq[Any]].mkString( "(", ", ", ")" )
							case t: SingleReferenceType if v ne null =>
								if (v.isInstanceOf[Int] || v.isInstanceOf[Long])
									s"$k = $v"
								else {
									val reft = t.asInstanceOf[SingleReferenceType].ref
									val refc = CommandFunctionHelpers.uniqueColumn( reft )

									s"$k = (SELECT id FROM ${reft.name} WHERE $refc = '$v')"
								}
							case _ => k + " = " + String.valueOf( v )
						}
					}) mkString ", "
			com ++= " WHERE id = "
			com ++= id.toString
			env.statement.executeUpdate( com.toString )
		}
	}

	def insertLinks( env: Environment, resource: Table, id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new BadRequestException( s"insertLinks: field not found: $field" )
			case Some( vs ) =>
				resource.columnMap(field).typ match {
					case ManyReferenceType( _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							associateID( env, resource, id, ref, CommandFunctionHelpers.uniqueColumn(ref), v )
					case _ => throw new BadRequestException( s"insertLinks: field not many-to-many: $field" )
				}
		}

	def append( env: Environment, resource: Table, id: Long, field: String, json: OBJ ) = {
		resource.columnMap get field match {
			case Some( Column(_, ManyReferenceType(_, ref), _, _, _, _) ) =>
				val tid = insert( env, ref, json )

				associateIDs( env, resource, id, ref, tid )
				tid
			case Some( _ ) => throw new BadRequestException( s"append: field not many-to-many: $field" )
			case None => throw new BadRequestException( s"append: field not found: $field" )
		}
	}

	def appendIDs( env: Environment, src: Table, sid: Long, field: String, tid: Long ) =
		src.columnMap get field match {
			case Some( Column(_, ManyReferenceType(_, ref), _, _, _, _) ) =>
				associateIDs( env, src, sid, ref, tid )
			case Some( _ ) => throw new BadRequestException( s"appendIDs: field not many-to-many: $field" )
			case None => throw new BadRequestException( s"appendIDs: field not found: $field" )
		}

	def deleteLinks( env: Environment, resource: Table, id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new BadRequestException( s"append: field not found: $field" )
			case Some( vs ) =>
				resource.columnMap(field).typ match {
					case ManyReferenceType( _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							deleteLinkID( env, resource, id, ref, CommandFunctionHelpers.uniqueColumn( ref ), v )
					case _ => throw new BadRequestException( s"append: field not many-to-many: $field" )
				}
		}

	def deleteLinksID( env: Environment, resource: Table, id: Long, field: String, tid: Long ) =
		resource.columnMap(field).typ match {
			case ManyReferenceType( _, ref ) => deleteLinkIDs( env, resource, id, ref, tid )
			case _ => throw new BadRequestException( s"append: field not many-to-many: $field" )
		}

	def deleteLinkID( env: Environment, src: Table, id: Long, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val did = QueryFunctions.findOne( env, dst, dfield, dvalue )( "id" ).asInstanceOf[Long]

		deleteLinkIDs( env, src, id, dst, did )
	}

	def deleteLinkIDs( env: Environment, src: Table, sid: Long, dst: Table, did: Long ) =
		command( env, s"DELETE FROM ${src.name}$$${dst.name} WHERE ${src.name}$$id = $sid AND ${dst.name}$$id = $did" )

	def associateID( env: Environment, src: Table, id: Long, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val did = QueryFunctions.findOne( env, dst, dfield, dvalue )( "id" ).asInstanceOf[Long]

		associateIDs( env, src, id, dst, did )
	}

	def associateIDs( env: Environment, src: Table, sid: Long, dst: Table, did: Long ) =
		command( env, s"INSERT INTO ${src.name}$$${dst.name} VALUES ($sid, $did)" )

	def associate( env: Environment, src: Table, sfield: String, svalue: AnyRef, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val sobj = QueryFunctions.findOne( env, src, sfield, svalue )( "id" ).asInstanceOf[Long]

		associateID( env, src, sobj, dst, dfield, dvalue )
	}
}
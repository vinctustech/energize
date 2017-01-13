package xyz.hyperreal.cras

import java.sql.Statement


object CommandFunctionHelpers {
	def insertCommand( env: Env, resource: Table, json: Map[String, AnyRef] ) = {
		val com = new StringBuilder( "INSERT INTO " )
		val json1 = escapeQuotes( json )

		com ++= resource.name
		com ++= resource.names.mkString( " (", ", ", ") " )
		com ++= "VALUES ("
		com ++=
			(for (c <- resource.names)
				yield {
					json1 get c match {
						case None => "NULL"
						case Some( v ) =>
							resource.columns(env.db.desensitize( c )).typ match {
								case ReferenceType( t ) if v != null && !v.isInstanceOf[Int] && !v.isInstanceOf[Long] =>
									s"(SELECT id FROM $t WHERE " +
										(env.tables(env.db.desensitize( t )).columns.values.find( c => c.unique ) match {
											case None => throw new CrasErrorException( "insert: no unique column in referenced resource in POST request" )
											case Some( c ) => c.name
										}) + " = '" + String.valueOf( v ) + "')"
								case StringType => '\'' + String.valueOf( v ) + '\''
								case _ => String.valueOf( v )
							}
					}
				}) mkString ", "
		com += ')'
		com.toString
	}
}

object CommandFunctions {
	def command( env: Env, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Env, resource: Table, id: Long ) = command( env, s"DELETE FROM ${resource.name} WHERE id = $id;" )
	
	def batchInsert( env: Env, resource: Table, rows: List[List[AnyRef]] ) {
		val types = for ((n, i) <- resource.names zipWithIndex) yield (i + 1, resource.columns(n.toUpperCase).typ)
			
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
	
	def insert( env: Env, resource: Table, json: Map[String, AnyRef] ) = {
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
	}
	
	def update( env: Env, resource: Table, json: Map[String, Any], id: Long, all: Boolean ) =
		if (all && json.keySet != resource.names.toSet)
			throw new CrasErrorException( "update: missing column(s) in PUT request" )
		else {
			val com = new StringBuilder( "UPDATE " )
			var typ: ColumnType = null
			
			com ++= resource.name
			com ++= " SET "
			com ++=
				escapeQuotes( json ).toList map {
					case (k, v) if resource.columns(k.toUpperCase).typ == StringType => k + " = '" + String.valueOf( v ) + "'"
					case (k, v) if {typ = resource.columns(k.toUpperCase).typ; typ.isInstanceOf[ReferenceType]} =>
						if (v.isInstanceOf[Int] || v.isInstanceOf[Long])
							k + " = " + String.valueOf( v )
						else {
							val reft = typ.asInstanceOf[ReferenceType].table
							val refc =
								(env.tables(reft.toUpperCase).columns.values.find( c => c.unique ) match {
									case None => throw new CrasErrorException( "update: no unique column in referenced resource in PUT/PATCH request" )
									case Some( c ) => c.name
								})

							k + s" = (SELECT id FROM $reft WHERE $refc = '$v')"
						}
					case (k, v) => k + " = " + String.valueOf( v )
				} mkString ", "
			com ++= " WHERE id = "
			com ++= id.toString
			env.statement.executeUpdate( com.toString )
		}
}
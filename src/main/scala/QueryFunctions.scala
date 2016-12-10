package xyz.hyperreal.cras

import collection.mutable.{ListBuffer}


object QueryFunctions {
	def query( env: Env, sql: String ) = {
		val res = env.statement.executeQuery( sql )
		val list = new ListBuffer[Map[String, Any]]
		val md = res.getMetaData
		val count = md.getColumnCount
		
		while (res.next)
			list +=
				Map(
					(for (i <- 1 to count) yield {
						val dbtable = md.getTableName(i)
						val dbcol = md.getColumnName(i)
						val col =
							env.tables get dbtable match {
								case None => dbcol
								case Some( t ) =>
									t.columns get dbcol match {
										case None =>
											if (dbcol.toLowerCase == "id")
												"id"
											else
												dbcol
										case Some( c ) => c.name
									}
							}
							
						(col, res.getObject(i))
					}): _* )
		
		list.toList
	}
	
	def list( env: Env, resource: Table ) = query( env, s"select * from ${resource.name};" )
	
	def find( env: Env, resource: Table, id: Long ) = query( env, s"select * from ${resource.name} where id = $id;" )
}

package xyz.hyperreal.cras

import collection.mutable.ListBuffer


object QueryFunctions {
	def query( env: Env, resource: Table, sql: String ) = {
		val res = env.statement.executeQuery( sql )
		val list = new ListBuffer[Map[String, Any]]
		val md = res.getMetaData
		val count = md.getColumnCount

		def mkmap( table: Table ): Map[String, Any] = {
			val attr = new ListBuffer[(String, Any)]
			
			for (i <- 1 to count) {
				val dbtable = md.getTableName( i )
				val dbcol = md.getColumnName( i )
				val obj = res.getObject( i )
				
				env.tables get dbtable match {
					case None => sys.error( "data not from a known table" )
					case Some( t ) if t == table =>
						t.columns get dbcol match {
							case None if dbcol.toLowerCase == "id" =>
								attr += ("id" -> obj)
							case None => sys.error( "data not from a known column" )
							case Some( Column(cname, TableType(reft), _, _, _, _) ) if obj ne null =>
								attr += (cname -> mkmap( env.tables(reft.toUpperCase) ))
							case Some( c ) =>
								attr += (c.name -> obj)
						}
					case _ =>
				}
			}
			
			Map( attr: _* )
		}
		
		while (res.next)
			list += mkmap( resource )
		
		list.toList
	}
	
	def list( env: Env, resource: Table ) =
		resource.columns.values.find( c => c.typ.isInstanceOf[TableType] ) match {
			case None =>
				query( env, resource, s"SELECT * FROM ${resource.name}" )
			case Some( Column(col, TableType(reft), _, _, _, _) ) =>
				query( env, resource, s"SELECT * FROM ${resource.name} LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id" )
		}
	
	def find( env: Env, resource: Table, id: Long ) =
		resource.columns.values.find( c => c.typ.isInstanceOf[TableType] ) match {
			case None =>
				query( env, resource, s"SELECT * FROM ${resource.name} WHERE id = $id" )
			case Some( Column(col, TableType(reft), _, _, _, _) ) =>
				query( env, resource,
					s"SELECT * FROM ${resource.name} LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id WHERE ${resource.name}.id = $id" )
		}
}

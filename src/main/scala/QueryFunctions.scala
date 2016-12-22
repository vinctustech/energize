package xyz.hyperreal.cras

import collection.mutable.ListBuffer

	
// 	def list( env: Env, resource: Table ) =
// 		resource.columns.values.find( c => c.typ.isInstanceOf[TableType] ) match {
// 			case None =>
// 				query( env, resource, s"SELECT * FROM ${resource.name}" )
// 			case Some( Column(col, TableType(reft), _, _, _, _) ) =>
// 				query( env, resource, s"SELECT * FROM ${resource.name} LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id" )
// 			case _ => throw new CrasErrorException( "shouldn't be impossible" )
// 		}

object QueryFunctionHelpers {
	def listQuery( resource: Table ) = {
		val buf = new StringBuilder( s"SELECT * FROM ${resource.name}" )
		
		resource.columns.values filter (c => c.typ.isInstanceOf[TableType]) foreach {
			case Column(col, TableType(reft), _, _, _, _) =>
				 buf ++= s" LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id"
		}
		
//		println( buf )
		buf.toString
	}
	
	val FILTER = "([a-zA-Z]+):(.+)"r
}

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
	
	def size( env: Env, resource: Table ) = {
		val res = env.statement.executeQuery( s"SELECT COUNT(*) FROM ${resource.name}" )
		
		res.next
		res.getInt( 1 )
	}

	def list( env: Env, resource: Table, filter: Option[String] ) = {
		val where =
			if (filter == None)
				""
			else {
				val QueryFunctionHelpers.FILTER(col, search) = filter.get
				val search1 = escapeQuotes( search )
				
				s" WHERE $col = '$search1'" 
			}
			
		query( env, resource, QueryFunctionHelpers.listQuery(resource) + where )
	}
	
	def find( env: Env, resource: Table, id: Long ) =
		query( env, resource, QueryFunctionHelpers.listQuery(resource) + s" WHERE ${resource.name}.id = $id" )
}

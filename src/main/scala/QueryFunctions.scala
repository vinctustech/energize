package xyz.hyperreal.cras

import collection.mutable.ListBuffer
import collection.immutable.ListMap

	
// 	def list( env: Env, resource: Table ) =
// 		resource.columns.values.find( c => c.typ.isInstanceOf[TableType] ) match {
// 			case None =>
// 				query( env, resource, s"SELECT * FROM ${resource.name}" )
// 			case Some( Column(col, TableType(reft), _, _, _, _) ) =>
// 				query( env, resource, s"SELECT * FROM ${resource.name} LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id" )
// 			case _ => throw new CrasErrorException( "shouldn't be impossible" )
// 		}

object QueryFunctionHelpers {
	val FILTER = "(.+)(=|<|>|<=|>=|!=|~)(.+)"r
	val ORDER = """(.+)\:(ASC|asc|DESC|desc)"""r
	val DELIMITER = ","r
	val NUMERIC = """[+-]?\d*\.?\d+(?:[eE][-+]?[0-9]+)?"""r
	
	def listQuery( db: Database, resource: Table, fields: Option[String] ) = {
		val fs =
			fields match {
				case None => Nil
				case Some( f ) => DELIMITER.split( f ).toList
			}
		val fss = fs.toSet
		val fssmid = fss - "id"
		
		if (fssmid.intersect( resource.names.toSet ) != fssmid)
			sys.error( "all fields must be apart of the resource definition" )
			
		val fs1 = if (fs == Nil) "*" else fs mkString ","
		val buf = new StringBuilder( s"SELECT $fs1 FROM ${resource.name}" )
		val fssd = fss map (f => db.desensitize( f ))

		def innerReferenceFieldJoin( tname: String, tref: Table ): Unit = {
			tref.columns.values foreach {
				case Column(col1, ReferenceType(tname1, tref1), _, _, _, _) =>
					buf ++= s" LEFT OUTER JOIN $tname1 ON $tname.$col1 = $tname1.id"
					innerReferenceFieldJoin( tname1, tref1 )
				case _ =>
			}
		}

		resource.columns.values foreach {
			case Column(col, ReferenceType(reft, reftref), _, _, _, _) if fssd.isEmpty || fssd(col) =>
				buf ++= s" LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id"
				innerReferenceFieldJoin( reft, reftref )
			case _ =>
		}
		
//		println( buf )
		buf.toString
	}
	
	def numeric( s: String ) = NUMERIC.pattern.matcher( s ).matches
}

object QueryFunctions {
	def query( env: Env, resource: Table, sql: String ) = {
		println( sql )
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
					case None => sys.error( s"data from an unknown table: $dbtable" )
					case Some( t ) if t == table =>
						t.columns get dbcol match {
							case None if dbcol.toLowerCase == "id" =>
								attr += ("id" -> obj)
							case None => sys.error( "data not from a known column" )
							case Some( Column(cname, ReferenceType(_, reft), _, _, _, _) ) if obj ne null =>
								attr += (cname -> mkmap( reft ))
							case Some( c ) =>
								attr += (c.name -> obj)
						}
					case _ =>
				}
			}
			
			ListMap( attr: _* )
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

	def list( env: Env, resource: Table,
		fields: Option[String], filter: Option[String], order: Option[String], page: Option[String], start: Option[String], limit: Option[String] ) = {
		val where =
			if (filter.isEmpty)
				""
			else {
				" WHERE " +
					(QueryFunctionHelpers.DELIMITER.split( filter.get ) map {
						f => {
							val QueryFunctionHelpers.FILTER(col, op, search) = f
							val search1 = escapeQuotes( search )
							
							if (op == "~")
								s"$col LIKE '$search1'"
							else if (QueryFunctionHelpers.numeric( search1 ))
								s"$col $op $search1"
							else
								s"$col $op '$search1'"
						}
					} mkString " AND ")
			}
		val orderby =
			if (order.isEmpty)
				""
			else {
				" ORDER BY " +
					(QueryFunctionHelpers.DELIMITER.split( order.get ) map {
						o => {
							val QueryFunctionHelpers.ORDER(col, ordering) = o
							val ordering1 = ordering.toUpperCase
							
							s"$col $ordering1"
						}
					} mkString ", ")
			}
			
		val limoff =
			(page map (p => p.toInt - 1), start, limit) match {
				case (None, None, None) => ""
				case (Some( p ), None, None) => s" LIMIT 10 OFFSET " + (p*10)
				case (None, Some( s ), None) => s" LIMIT 10 OFFSET $s"
				case (Some( _ ), Some( _ ), _) => sys.error( "'page' and 'start' can't be used in the same query" )
				case (None, None, Some( l )) => s" LIMIT $l"
				case (Some( p ), None, Some( l )) => s" LIMIT $l OFFSET " + (p*l.toInt)
				case (None, Some( s ), Some( l )) => s" LIMIT $l OFFSET $s"
			}
			
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, fields) + where + orderby + limoff )
	}
	
	def find( env: Env, resource: Table, id: Long, fields: Option[String] ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, fields) + s" WHERE ${resource.name}.id = $id" )
}

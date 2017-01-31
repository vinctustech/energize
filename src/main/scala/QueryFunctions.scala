package xyz.hyperreal.energize

import collection.mutable.ListBuffer
import collection.immutable.ListMap

import xyz.hyperreal.lia.Math

	
// 	def list( env: Env, resource: Table ) =
// 		resource.columns.values.find( c => c.typ.isInstanceOf[TableType] ) match {
// 			case None =>
// 				query( env, resource, s"SELECT * FROM ${resource.name}" )
// 			case Some( Column(col, TableType(reft), _, _, _, _) ) =>
// 				query( env, resource, s"SELECT * FROM ${resource.name} LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id" )
// 			case _ => throw new EnergizeErrorException( "shouldn't be impossible" )
// 		}

object QueryFunctionHelpers {
	val FILTER = "(.+)(=|<|>|<=|>=|!=|~)(.+)"r
	val ORDER = """(.+)\:(ASC|asc|DESC|desc)"""r
	val DELIMITER = ","r
	val NUMERIC = """[+-]?\d*\.?\d+(?:[eE][-+]?[0-9]+)?"""r
	
	def listQuery( db: Database, resource: Table, fields: Option[String], where: String, page: Option[String], start: Option[String], limit: Option[String] ) = {
		val fs =
			fields match {
				case None => Nil
				case Some( f ) =>
					DELIMITER.split( f ).toList
			}
		val fss = fs.toSet
		val fssmid = fss - "id"
		
		if (fssmid.intersect( resource.columns map (c => c.name) toSet ) != fssmid)
			sys.error( "all fields must be apart of the resource definition" )
			
		val fs1 =
			if (resource.mtm) {
				val cs =
					if (fs == Nil)
						resource.columns
					else
						fs map (f => resource.columnMap( db.desensitize(f) ))

				(cs map {
						case Column(f, ManyReferenceType(_, _), _, _, _, _) => s"null as $f"
						case Column(f, _, _, _, _, _) => f
					}) :+ "id" mkString ","
			} else if (fs == Nil)
				"*"
			else
				fs mkString ","
		val buf = new StringBuilder( s"SELECT $fs1 FROM ${resource.name}" )
		val fssd = fss map (f => db.desensitize( f ))

		def innerReferenceFieldJoin( tname: String, tref: Table ): Unit = {
			tref.columns foreach {
				case Column(col1, SingleReferenceType(tname1, tref1), _, _, _, _) =>
					buf ++= s" LEFT OUTER JOIN $tname1 ON $tname.$col1 = $tname1.id"
					innerReferenceFieldJoin( tname1, tref1 )
				case _ =>
			}
		}

		resource.columns foreach {
			case Column(col, SingleReferenceType(reft, reftref), _, _, _, _) if fssd.isEmpty || fssd(col) =>
				buf ++= s" LEFT OUTER JOIN $reft ON ${resource.name}.$col = $reft.id"
				innerReferenceFieldJoin( reft, reftref )
			case _ =>
		}

		buf ++= where
		buf ++= pageStartLimit( page, start, limit )
		buf.toString
	}

	def pageStartLimit( page: Option[String], start: Option[String], limit: Option[String] ) =
		(page map (p => p.toInt - 1), start, limit) match {
			case (None, None, None) => ""
			case (Some( p ), None, None) => s" LIMIT 10 OFFSET " + (p*10)
			case (None, Some( s ), None) => s" LIMIT 10 OFFSET $s"
			case (Some( _ ), Some( _ ), _) => sys.error( "'page' and 'start' can't be used in the same query" )
			case (None, None, Some( l )) => s" LIMIT $l"
			case (Some( p ), None, Some( l )) => s" LIMIT $l OFFSET " + (p*l.toInt)
			case (None, Some( s ), Some( l )) => s" LIMIT $l OFFSET $s"
		}

	def numeric( s: String ) = NUMERIC.pattern.matcher( s ).matches
}

object QueryFunctions {
	def query( env: Environment, resource: Table, sql: String, page: Option[String], start: Option[String], limit: Option[String] ): List[OBJ] = {
		val res = new Relation( env.statement.executeQuery(sql) )
		val list = new ListBuffer[OBJ]

		def mkOBJ( table: Table ): OBJ = {
			val attr = new ListBuffer[(String, AnyRef)]

			for (i <- 0 until res.columnCount) {
				val dbtable = res.columns(i).table
				val dbcol = res.columns(i).name
				val obj = res.get( i )

				if (dbtable == "")
					table.columnMap get dbcol match {
						case None => attr += (dbcol -> obj)
						case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
							attr += (cname -> query( env, reft,
								s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.id " +
									s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(env.db.desensitize("id"))}" +
									QueryFunctionHelpers.pageStartLimit(page, start, limit), page, start, limit ))
						case Some( c ) => sys.error( s"data not from a table: matching column: ${c.name}" )
					}
				else
					env.tables get dbtable match {
						case None if !(dbtable contains '$') => sys.error( s"data from an unknown table: $dbtable" )
	//					case None =>
	//						table.columns get dbcol match {
	//							case None => attr += (dbcol -> obj)
	//							case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
	//								attr += (cname -> query( env, reft,
	//									s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.id " +
	//										s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(env.db.desensitize("id"))}" ))
	//							case Some( c ) => attr += (c.name -> obj)
	//						}
						case Some( t ) if t == table =>
							t.columnMap get dbcol match {
								case None if dbcol.toLowerCase == "id" => attr += ("id" -> obj)
								case None => sys.error( s"data from an unknown column: $dbcol" )
								case Some( Column(cname, SingleReferenceType(_, reft), _, _, _, _) ) if obj ne null =>
									attr += (cname -> mkOBJ( reft ))
								case Some( Column(cname, ArrayType(typ, p, _, d), _, _, _, _) ) if obj ne null =>
									attr += (cname -> obj.asInstanceOf[Array[AnyRef]].toList)
//									attr += (cname -> obj.asInstanceOf[java.sql.Array].getArray.asInstanceOf[Array[AnyRef]].toList)
								case Some( Column(cname, DatetimeType|TimestampType, _, _, _, _) ) if obj ne null =>
									attr += (cname -> env.db.writeTimestamp( obj ))
								case Some( c ) => attr += (c.name -> obj)
							}
						case _ =>
					}
			}
			
			ListMap( attr: _* )
		}

		while (res.next)
			list += mkOBJ( resource )

		list.toList
	}
	
	def size( env: Environment, resource: Table ) =
		Math.maybePromote( query(env, resource, s"SELECT COUNT(*) FROM ${resource.name}", None, None, None).head.values.head.asInstanceOf[Long] )

	def list( env: Environment, resource: Table,
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

		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, fields, where + orderby, page, start, limit), Some("1"), None, None )
	}
	
	def findID( env: Environment, resource: Table, id: Long, fields: Option[String], page: Option[String], start: Option[String], limit: Option[String] ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, fields, s" WHERE ${resource.name}.id = $id",
			page, start, limit), None, None, None )

	def findIDMany( env: Environment, resource: Table, id: Long, fields: String, page: Option[String], start: Option[String], limit: Option[String] ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, Some(fields), s" WHERE ${resource.name}.id = $id",
			None, None, None), page, start, limit )

	def findValue( env: Environment, resource: Table, field: String, value: Any ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, None, s" WHERE ${resource.name}.$field = '$value'",
			None, None, None), None, None, None )

	def findOne( env: Environment, resource: Table, field: String, value: Any ) = findValue( env, resource, field, value ).head

	def findOption( env: Environment, resource: Table, field: String, value: Any ) = findValue( env, resource, field, value ).headOption
}

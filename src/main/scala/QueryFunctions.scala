package xyz.hyperreal.energize

import java.sql.{Blob, Date, Time}

import collection.mutable.ListBuffer
import collection.immutable.ListMap


object QueryFunctionHelpers {
	val FILTER = "(.+)(=|<|>|<=|>=|!=|~)(.+)"r
	val ORDER = """(.+)\:(ASC|asc|DESC|desc)"""r
	val DELIMITER = ","r
	val NUMERIC = """[+-]?\d*\.?\d+(?:[eE][-+]?[0-9]+)?"""r
	
	def listQuery( db: Database, resource: Table, fields: Option[String], where: String, page: Option[String], start: Option[String],
								 limit: Option[String] ) = {
		val fs =
			fields match {
				case None => Nil
				case Some( f ) =>
					DELIMITER.split( f ).toList
			}
		val fss = fs.toSet
		val fssmid = fss - "id"
		
		if (fssmid.intersect( resource.columns map (_.name) toSet ) != fssmid)
			sys.error( "all fields must be apart of the resource definition" )
			
//		val fs1 =
//			if (resource.mtm) {
//				val cs =
//					if (fs == Nil)
//						resource.columns
//					else
//						fs map (f => resource.columnMap( db.desensitize(f) ))
//
//				(cs map {
//						case Column(f, ManyReferenceType(_, _), _, _, _, _) => s"null as $f"
//						case Column(f, _, _, _, _, _) => f
//					}) :+ s"${resource.name}.id" mkString ","
//			} else if (fs == Nil)
//				"*"
//			else
//				fs mkString ","
		val buf = new StringBuilder( s"SELECT * FROM ${resource.name}" )
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
		Query( buf.toString, fs )
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
	def query( env: Environment, resource: Table, sql: Query, page: Option[String], start: Option[String], limit: Option[String],
						 allowsecret: Boolean ): List[OBJ] = {
		val res = new Relation( env, env.statement.executeQuery(sql.query) )
		val list = new ListBuffer[OBJ]

		def mkOBJ( table: Table, fields: List[String] ): OBJ = {
			val attr = new ListBuffer[(String, AnyRef)]
			val cols =
				if (fields == Nil)
					"id" +: table.names
				else
					fields
			for (cn <- cols) {
				val obj = res.get( table.name, cn )

//				if (dbtable == "")
//					table.columnMap get dbcol match {
//						case None =>
//							attr += (dbcol -> obj)
//						case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
//							attr += (cname -> query( env, reft,
//								Query(s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.id " +
//									s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(table.name, "id")}" +
//									QueryFunctionHelpers.pageStartLimit(page, start, limit)), page, start, limit, allowsecret ))
//						case Some( c ) => sys.error( s"data not from a table: matching column: ${c.name}" )
//					}
//				else

//					env.tables get dbtable match {
//						case None if !(dbtable contains '$') => sys.error( s"data from an unknown table: $dbtable" )
//					case None =>
//						table.columns get dbcol match {
//							case None => attr += (dbcol -> obj)
//							case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
//								attr += (cname -> query( env, reft,
//									s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.id " +
//										s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(env.db.desensitize("id"))}" ))
//							case Some( c ) => attr += (c.name -> obj)
//						}
//						case Some( t ) if t == table =>
							table.columnMap get cn match {
								case None if cn == "id" => attr += ("id" -> obj.get)
								case None => sys.error( s"data from an unknown column: $cn" )
								case Some( Column(cname, SingleReferenceType(_, reft), _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> mkOBJ( reft, Nil ))
								case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
									attr += (cname -> query( env, reft,
										Query(s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.id " +
											s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(table.name, "id")}" +
											QueryFunctionHelpers.pageStartLimit(page, start, limit)), page, start, limit, allowsecret ))
								case Some( Column(cname, ArrayType(_, _, _, _), _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.asInstanceOf[Array[AnyRef]].toList)
//									attr += (cname -> obj.asInstanceOf[java.sql.Array].getArray.asInstanceOf[Array[AnyRef]].toList)
								case Some( Column(cname, BinaryType, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.asInstanceOf[Array[Byte]].map( byte2hex ).mkString)
								case Some( Column(cname, BLOBType(rep), _, _, _, _) ) if obj.get ne null =>
									val blob = obj.get.asInstanceOf[Blob]
									val array = blob.getBytes( 0L, blob.length.toInt )

									rep match {
										case 'base64 => attr += (cname -> bytes2base64( array ))
										case 'hex => attr += (cname -> array.map( byte2hex ).mkString)
										case 'list => attr += (cname -> array.toList)
									}
								case Some( Column(cname, MediaType(_, _, _), _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> s"/media/${obj.get}")
								case Some( Column(cname, DatetimeType|TimestampType, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> env.db.writeTimestamp( obj.get ))
								case Some( Column(cname, EnumType(enum), _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> enum(obj.get.asInstanceOf[Int]))
								case Some( Column(cname, DateType, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.asInstanceOf[Date].toString)
								case Some( Column(cname, TimeType, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.asInstanceOf[Time].toString)
								case Some( Column(cname, _, true, _, _, _) ) =>
									if (allowsecret)
										attr += (cname -> obj.get)
								case Some( c ) =>
									attr += (c.name -> obj.get)
					}
			}
			
			ListMap( attr: _* )
		}

		while (res.next)
			list += mkOBJ( resource, sql.fields )

		list.toList
	}

	def size( env: Environment, resource: Table ) = {
		val res = env.statement.executeQuery( s"SELECT COUNT(*) FROM ${resource.name}" )

		res.next

		BigInt( res.getLong(1) )
	}

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

		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, fields, where + orderby, page, start, limit), Some("1"), None, None, false )
	}

	def findID( env: Environment, resource: Table, id: Long, fields: Option[String], page: Option[String], start: Option[String], limit: Option[String] ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, fields, s" WHERE ${resource.name}.id = $id",
			page, start, limit), None, None, None, false )

	def findIDMany( env: Environment, resource: Table, id: Long, fields: String, page: Option[String], start: Option[String], limit: Option[String] ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, Some(fields), s" WHERE ${resource.name}.id = $id",
			None, None, None), page, start, limit, false )

	def findValue( env: Environment, resource: Table, field: String, value: Any, allowsecret: Boolean ) =
		query( env, resource, QueryFunctionHelpers.listQuery(env.db, resource, None, s" WHERE ${resource.name}.$field = '$value'",
			None, None, None), None, None, None, allowsecret )

	def findOne( env: Environment, resource: Table, field: String, value: Any ) = findValue( env, resource, field, value, false ).head

	def findOption( env: Environment, resource: Table, field: String, value: Any, allowsecret: Boolean ) =
		findValue( env, resource, field, value, allowsecret ).headOption

//	def findField( env: Environment, resource: String, id: Long, field: String ) =
//		query( env, env.tables(env.db.desensitize(resource)), s"SELECT $field FROM $resource WHERE id = $id", None, None, None, false ).head( field )

	def readBlob( env: Environment, resource: String, id: Long, field: String ) = {
		val res = env.statement.executeQuery( s"SELECT $field FROM $resource WHERE id = $id" )

		res.next

		val blob = res.getBlob( 1 )

		blob.getBytes( 0, blob.length.toInt )
	}

	def readMedia( env: Environment, id: Long ) = {
		val res = env.statement.executeQuery( s"SELECT * FROM _media_ WHERE id = $id" )

		res.next

		val typ = res.getString( 2 )
		val blob = res.getBlob( 3 )

		(typ, blob.getBytes( 0, blob.length.toInt ))
//		Map(
//			"type" -> typ,
//			"data" -> blob.getBytes( 0, blob.length.toInt ))
	}
}

case class Query( query: String, fields: List[String] = Nil )
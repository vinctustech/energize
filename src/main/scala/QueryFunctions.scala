package xyz.hyperreal.energize2

import java.sql.{Blob, Clob, Statement}

import xyz.hyperreal.bvm.VM

import collection.mutable.ListBuffer
import collection.immutable.ListMap


object QueryFunctionHelpers {
	val FILTER = """(.+)(=|<=|>=|<|>|!=|~\*|~)(.+)"""r
	val ORDER = """(.+)\:(ASC|asc|DESC|desc)"""r
	val DELIMITER = ","r
	val NUMERIC = """[+-]?\d*\.?\d+(?:[eE][-+]?[0-9]+)?"""r

	def filtering( filter: Option[String] ) =
		if (filter.isEmpty)
			""
		else {
			" WHERE " +
				(QueryFunctionHelpers.DELIMITER.split( filter.get ) map {
					f => {
						val QueryFunctionHelpers.FILTER(col, op, search) = f
						val search1 = escapeQuotes( search )

						if (op == "~")
							s"${nameIn(col)} LIKE '$search1'"
						else if (op == "~*")
							s"${nameIn(col)} ILIKE '$search1'"
						else if (QueryFunctionHelpers.numeric( search1 ))
							s"${nameIn(col)} $op $search1"
						else
							s"${nameIn(col)} $op '$search1'"
					}
				} mkString " AND ")
		}

	def listQuery( db: Database, resource: Resource, fields: Option[String], where: String, page: Option[String], start: Option[String],
								 limit: Option[String] ) = {
		val fs = {
			val fs1 =
				fields match {
					case None => Nil
					case Some( f ) =>
						DELIMITER.split( f ).toList
				}
			val exclude = fs1 filter (_ startsWith "-") map (_ substring 1)

			if (exclude nonEmpty) {
				val include = fs1 filterNot (_ startsWith "-")

				if (include nonEmpty)
					include filterNot (exclude contains _)
				else
					resource.names filterNot (exclude contains _)
			} else
				fs1
		}
		val fss = fs.toSet
		val fssmid = fss - "_id"
		
		if (fssmid.intersect( resource.fields map (_.name) toSet ) != fssmid)
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

		def innerReferenceFieldJoin( tname: String, tref: Resource ): Unit = {
			tref.fields foreach {
				case Field(col1, SingleReferenceType(pos, tname1, ref), _, _, _, _, _) =>
					buf ++= s" LEFT OUTER JOIN $tname1 ON $tname.${nameIn(col1)} = $tname1.$idIn"
					innerReferenceFieldJoin( tname1, ref )
				case _ =>
			}
		}

		resource.fields foreach {
			case Field(col, SingleReferenceType(pos, reft, ref), _, _, _, _, _) if fssd.isEmpty || fssd(col) =>
				buf ++= s" LEFT OUTER JOIN $reft ON ${resource.name}.${nameIn(col)} = $reft.$idIn"
				innerReferenceFieldJoin( reft, ref )
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
	def query( resource: Resource, sql: Query, page: Option[String], start: Option[String], limit: Option[String],
						 allowsecret: Boolean ): List[OBJ] = {
		val res = QueryFunctionHelpers.synchronized( new Relation( resource.db, resource.statement.executeQuery(sql.query) ) )
		val list = new ListBuffer[OBJ]

		def mkOBJ( table: Resource, fields: List[String] ): OBJ = {
			val attr = new ListBuffer[(String, AnyRef)]
			val cols =
				if (fields == Nil)
					"_id" +: table.names
				else
					fields
			for (cn <- cols) {
				val obj = res.get( table.name, cn )

//				if (dbtable == "")
//					table.columnMap get dbcol match {
//						case None =>
//							attr += (dbcol -> obj)
//						case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
//							attr += (cname -> query( vm, reft,
//								Query(s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref._id " +
//									s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(table.name, "_id")}" +
//									QueryFunctionHelpers.pageStartLimit(page, start, limit)), page, start, limit, allowsecret ))
//						case Some( c ) => sys.error( s"data not from a table: matching column: ${c.name}" )
//					}
//				else

//					vm.tables get dbtable match {
//						case None if !(dbtable contains '$') => sys.error( s"data from an unknown table: $dbtable" )
//					case None =>
//						table.columns get dbcol match {
//							case None => attr += (dbcol -> obj)
//							case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
//								attr += (cname -> query( vm, reft,
//									s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref._id " +
//										s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(vm.db.desensitize("_id"))}" ))
//							case Some( c ) => attr += (c.name -> obj)
//						}
//						case Some( t ) if t == table =>
							table.fieldMap get cn match {
								case None if cn == "_id" => attr += ("_id" -> obj.get)
								case None => sys.error( s"data from an unknown column: $cn" )
								case Some( Field(cname, SingleReferenceType(_, _, reft), _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> mkOBJ( reft, Nil ))
								case Some( Field(cname, ManyReferenceType(_, ref, reft), _, _, _, _, _) ) =>
									attr += (cname -> query( reft,
										Query(s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.$idIn " +
											s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(table.name, "_id")}" +
											QueryFunctionHelpers.pageStartLimit(page, start, limit)), page, start, limit, allowsecret ))
								case Some( Field(cname, ArrayType(MediaType(_)), _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.asInstanceOf[Array[AnyRef]].toList.map( m => s"/media/$m" ))
								case Some( Field(cname, ArrayType(_), _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.asInstanceOf[Array[AnyRef]].toList)
								case Some( Field(cname, BinaryType, _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> bytes2hex( obj.get.asInstanceOf[Array[Byte]] ))
								case Some( Field(cname, BLOBType(rep), _, _, _, _, _) ) if obj.get ne null =>
									val blob = obj.get.asInstanceOf[Blob]
									val array = blob.getBytes( 0L, blob.length.toInt )

									rep match {
										case 'base64 => attr += (cname -> bytes2base64( array ))
										case 'hex => attr += (cname -> bytes2hex( array ))
										case 'list => attr += (cname -> array.toList)
									}
								case Some( Field(cname,TextType, _, _, _, _, _) ) if obj.get ne null =>
									val clob = obj.get.asInstanceOf[Clob]
									val s = clob.getSubString( 1L, clob.length.toInt )

									attr += (cname -> s)
								case Some( Field(cname, MediaType(_), _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> s"/media/${obj.get}")
								case Some( Field(cname, DatetimeType|TimestampType, _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> resource.db.writeTimestamp( obj.get ))
								case Some( Field(cname, EnumType(_, enum), _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> enum(obj.get.asInstanceOf[Int]))
								case Some( Field(cname, DateType|TimeType|UUIDType, _, _, _, _, _) ) if obj.get ne null =>
									attr += (cname -> obj.get.toString)
								case Some( Field(cname, _, true, _, _, _, _) ) =>
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

	def list( vm: VM, resource: Resource,
						fields: Option[String], filter: Option[String], order: Option[String], page: Option[String], start: Option[String], limit: Option[String] ) = {
		val where = QueryFunctionHelpers.filtering( filter )
		val orderby =
			if (order.isEmpty)
				""
			else {
				" ORDER BY " +
					(QueryFunctionHelpers.DELIMITER.split( order.get ) map {
						o => {
							val QueryFunctionHelpers.ORDER(col, ordering) = o
							val ordering1 = ordering.toUpperCase
							
							s"${nameIn(col)} $ordering1"
						}
					} mkString ", ")
			}

		query( resource, QueryFunctionHelpers.listQuery(vm.db, resource, fields, where + orderby, page, start, limit), Some("1"), None, None, false )
	}

	def findID( resource: Resource, id: Long, fields: Option[String], page: Option[String], start: Option[String], limit: Option[String] ) =
		query( resource, QueryFunctionHelpers.listQuery(resource.db, resource, fields, s" WHERE ${resource.name}.$idIn = $id",
			page, start, limit), None, None, None, false )

	def findIDMany( resource: Resource, id: Long, fields: String, page: Option[String], start: Option[String], limit: Option[String] ) =
		query( resource, QueryFunctionHelpers.listQuery(resource.db, resource, Some(fields), s" WHERE ${resource.name}.$idIn = $id",
			None, None, None), page, start, limit, false )

	def findValue( resource: Resource, field: String, value: Any, allowsecret: Boolean ) =
		query( resource, QueryFunctionHelpers.listQuery(resource.db, resource, None, s" WHERE ${resource.name}.${nameIn(field)} = '$value'",
			None, None, None), None, None, None, allowsecret )

	def findOne( resource: Resource, field: String, value: Any ) = findValue( resource, field, value, false ).head

	def findOption( resource: Resource, field: String, value: Any, allowsecret: Boolean ) =
		findValue( resource, field, value, allowsecret ).headOption

	def findField( resource: Resource, id: Long, field: String ) =
		query( resource, Query(s"SELECT ${nameIn(field)} FROM ${resource.name} WHERE $idIn = $id", List(field)), None, None, None, false ).head( field )

	def readField( resource: Resource, id: Long, field: String ) = {
		val res = resource.statement.executeQuery( s"SELECT ${nameIn(field)} FROM ${resource.name} WHERE $idIn = $id" )

		res.next
		res.getObject( 1 )
	}

//	def readBlob( resource: String, id: Long, field: String ) = {
//		val res = resource.statement.executeQuery( s"SELECT ${nameIn(field)} FROM $resource WHERE $idIn = $id" )
//
//		res.next
//
//		val blob = res.getBlob( 1 )
//
//		blob.getBytes( 0, blob.length.toInt )
//	}

	def readMedia( resource: Resource, id: Long ) = {
		val res = resource.statement.executeQuery( s"SELECT * FROM _media_ WHERE $idIn = $id" )

		res.next

		val typ = res.getString( 2 )
		val blob = res.getBlob( 3 )

		(typ, blob.getBytes( 0, blob.length.toInt ))
	}
}

case class Query( query: String, fields: List[String] = Nil )
package xyz.hyperreal.energize


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

	def listQuery( resource: Resource, fields: Option[String], where: String, page: Option[String], start: Option[String],
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
		val fssd = fss map (f => resource.processor.db.desensitize( f ))

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

case class Query( query: String, fields: List[String] = Nil )
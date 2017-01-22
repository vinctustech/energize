package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val constants =
		List(
			"i" -> ComplexBigInt.i,
			"e" -> math.E,
			"pi" -> math.Pi,
			"None" -> None
		)
	
	def pairs( natives: List[Native] ) = natives map (n => n.name -> n)
	
	val map =
		pairs( Native(QueryFunctions) ) ++
		pairs( Native(CommandFunctions) ) ++
		pairs( Native(ResultFunctions) ) ++
		pairs( Native(SupportFunctions) ) ++
		constants toMap
	
	val routes =
		"""
		|route <base>/<resource>
		|  GET     /id:long                  dataResult( "<resource>", singleOrNotFound(findID(<resource>, id, ?fields, None, None, None)) )
		|  GET     /                         dataResult( "<resource>", list(<resource>, ?fields, ?filter, ?order, ?page, ?start, ?limit) )
		|  POST    /                         dataResult( "<resource>", insert(<resource>, json) )
		|  PATCH   /id:long                  atLeastOneOrNotFound( update(<resource>, id, json, false) ); null
		|  PUT     /id:long                  atLeastOneOrNotFound( update(<resource>, id, json, true) ); null
		|  DELETE  /id:long                  atLeastOneOrNotFound( delete(<resource>, id) ); null
		""".stripMargin

	val mtmroutes =
		"""
		|route <base>/<resource>
		|  GET     /id:long/field:           dataResult( "<resource>", singleOrNotFound(findIDMany(<resource>, id, field, ?page, ?start, ?limit)) )
		|  POST    /id:long/field:           dataResult( "<resource>", append(<resource>, id, field, json) )
		|  DELETE  /id:long/field:           deleteLinks( <resource>, id, field, json )
		|  DELETE  /id:long/field:/tid:long  atLeastOneOrNotFound( deleteLinksID(<resource>, id, field, tid) ); null
		""".stripMargin

	//insertLinks(<resource>, id, field, json)

	val control = io.Source.fromString(
		"""
		|route /control
		|  DELETE  /res:     dataResult( res, deleteResource(res) )
		""".stripMargin )
}

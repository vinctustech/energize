package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val constants =
		List(
			"i" -> ComplexBigInt.i,
			"e" -> math.E,
			"pi" -> math.Pi
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
		|  GET     /id:long  dataResult( "<resource>", singleOrNotFound(findId(<resource>, id, ?fields)) )
		|  GET     /         dataResult( "<resource>", list(<resource>, ?fields, ?filter, ?order, ?page, ?start, ?limit) )
		|  POST    /         dataResult( "<resource>", insert(<resource>, json) )
		|  PATCH   /id:long  atLeastOneOrNotFound( update(<resource>, json, id, false) ); null
		|  PUT     /id:long  atLeastOneOrNotFound( update(<resource>, json, id, true) ); null
		|  DELETE  /id:long  atLeastOneOrNotFound( delete(<resource>, id) ); null
		""".stripMargin

	val control = io.Source.fromString(
		"""
		|route /control
		|  DELETE  /res:     dataResult( res, deleteResource(res) )
		""".stripMargin )
}

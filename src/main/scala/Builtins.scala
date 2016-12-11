package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val constants =
		List(
			"i" -> ComplexBigInt.i
		)
	
	def pairs( natives: List[Native] ) = natives map (n => (n.name -> n))
	
	val map =
		pairs( Native(QueryFunctions) ) ++
		pairs( Native(CommandFunctions) ) ++
		pairs( Native(ResultFunctions) ) ++
		pairs( Native(SupportFunctions) ) ++
		constants toMap
	
	val routes =
		"""
		|route <base>/<resource>
		|  GET    /:id    dataResult( "<resource>", singleOrNotFound(find(<resource>, long(id))) )
		|  GET    /       dataResult( "<resource>", list(<resource>) )
		|  POST   /       dataResult( "<resource>", insert(<resource>, json) )
		|  PATCH  /:id    atLeastOneOrNotFound( update(<resource>, json, long(id), false) ); null
		|  PUT    /:id    atLeastOneOrNotFound( update(<resource>, json, long(id), true) ); null
		|  DELETE /:id    atLeastOneOrNotFound( delete(<resource>, long(id)) ); null
		""".stripMargin
}

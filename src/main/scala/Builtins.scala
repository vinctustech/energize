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
		|  PATCH  /:id    dataResult( "<resource>", atLeastOneOrNotFound(update(<resource>, json, long(id), false)) )
		|  PUT    /:id    dataResult( "<resource>", atLeastOneOrNotFound(update(<resource>, json, long(id), true)) )
		|  DELETE /:id    dataResult( "<resource>", atLeastOneOrNotFound(delete(<resource>, long(id))) )
		""".stripMargin
}

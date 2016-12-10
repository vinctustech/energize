package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val natives =
		List(
			InsertNative, UpdateNative,
			AtLeastOneOrNotFoundNative
		)
// 	val natives2 =
// 		List(
// 			EvalNative
// 		)
	val constants =
		List(
			"i" -> ComplexBigInt.i
		)
	
	def pairs( natives: List[Native2] ) = natives map (n => (n.name -> n))
	
	val map =
		pairs( Native(QueryFunctions) ) ++
		pairs( Native(CommandFunctions) ) ++
		pairs( Native(ResultFunctions) ) ++
		pairs( Native(SupportFunctions) ) ++
		(natives map (n => (n.name -> n))) ++
// 		(natives2 map (n => (n.name -> n))) ++
		constants toMap
	
	val routes =
		"""
		|route <base>/<resource>
		|  GET    /:id    dataResult( "<resource>", singleOrNotFound(find(<resource>, id)) )
		|  GET    /       dataResult( "<resource>", list(<resource>) )
		|  POST   /       dataResult( "<resource>", insert(<resource>, json) )
		|  PATCH  /:id    dataResult( "<resource>", atLeastOneOrNotFound(update(<resource>, json, id, false)) )
		|  PUT    /:id    dataResult( "<resource>", atLeastOneOrNotFound(update(<resource>, json, id, true)) )
		|  DELETE /:id    dataResult( "<resource>", atLeastOneOrNotFound(delete(<resource>, id)) )
		""".stripMargin
}

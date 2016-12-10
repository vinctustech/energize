package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val natives =
		List(
			InsertNative, UpdateNative,
			SingleOrNotFoundNative, AtLeastOneOrNotFoundNative,
			IntNative, EvalNative, PrintNative
		)
	val constants =
		List(
			"i" -> ComplexBigInt.i
		)
	
	def pairs( natives: List[Native2] ) = natives map (n => (n.name -> n))
	
	val map = pairs( Native(Queries) ) ++ pairs( Native(Commands) ) ++ pairs( Native(Results) ) ++ (natives map (n => (n.name -> n))) ++ constants toMap
	
	val routes =
		"""
		|route <base>/<resource>
		|  GET    /:id    OK( "<resource>", singleOrNotFound(find(<resource>, id)) )
		|  GET    /       OK( "<resource>", list(<resource>) )
		|  POST   /       OK( "<resource>", insert(<resource>, json) )
		|  PATCH  /:id    OK( "<resource>", atLeastOneOrNotFound(update(<resource>, json, id, false)) )
		|  PUT    /:id    OK( "<resource>", atLeastOneOrNotFound(update(<resource>, json, id, true)) )
		|  DELETE /:id    OK( "<resource>", atLeastOneOrNotFound(delete(<resource>, id)) )
		""".stripMargin
}

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
		|  GET    /:id    OK( singleOrNotFound(find(<resource>, id)) )
		|  GET    /       OK( list(<resource>) )
		|  POST   /       OK( insert(<resource>, json) )
		|  PATCH  /:id    OK( atLeastOneOrNotFound(update(<resource>, json, id, false)) )
		|  PUT    /:id    OK( atLeastOneOrNotFound(update(<resource>, json, id, true)) )
		|  DELETE /:id    OK( atLeastOneOrNotFound(delete(<resource>, id)) )
		""".stripMargin
}

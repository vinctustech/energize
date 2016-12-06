package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val natives =
		List(
			QueryNative, InsertNative, UpdateNative, CommandNative,
			SingleOrNotFoundNative, AtLeastOneOrNotFoundNative,
			IntNative, EvalNative, PrintNative, OKNative, ErrorNative
		)
	val constants =
		List(
			"i" -> ComplexBigInt.i
		)
	
	def map = (natives map (n => (n.name -> n))) ++ constants toMap
	
	val routes =
		"""
		|route <base>/<resource>
		|  GET    /:id    OK( singleOrNotFound(query("select * from <resource> where id = '$id';")) )
		|  GET    /       OK( query("select * from <resource>;") )
		|  POST   /       OK( insert(<resource>, json) )
		|  PATCH  /:id    OK( atLeastOneOrNotFound(update(<resource>, json, id, false)) )
		|  PUT    /:id    OK( atLeastOneOrNotFound(update(<resource>, json, id, true)) )
		|  DELETE /:id    OK( atLeastOneOrNotFound(command("delete from <resource> where id = '$id';")) )
		""".stripMargin
}

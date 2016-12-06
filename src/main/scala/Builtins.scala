package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val natives =
		List(
			QueryNative, InsertNative, UpdateNative, CommandNative,
			SingleOrNotFoundNative, AtLeastOneOrNotFoundNative,
			IntNative, EvalNative, PrintNative
		)
	val constants =
		List(
			"i" -> ComplexBigInt.i
		)
	
	def map = (natives map (n => (n.name -> n))) ++ constants toMap
	
	val routes =
		"""
		|route <base>/<table>
		|  GET    /:id    OK( singleOrNotFound(query("select * from <table> where id = '$id';")) )
		|  GET    /       OK( query("select * from <table>;") )
		|  POST   /       OK( insert(<table>, json) )
		|  PATCH  /:id    OK( atLeastOneOrNotFound(update(<table>, json, id, false)) )
		|  PUT    /:id    OK( atLeastOneOrNotFound(update(<table>, json, id, true)) )
		|  DELETE /:id    OK( atLeastOneOrNotFound(command("delete from <table> where id = '$id';")) )
		""".stripMargin
}

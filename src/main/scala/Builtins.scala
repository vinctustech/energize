package xyz.hyperreal.cras

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val natives =
		List(
			QueryNative, InsertNative, UpdateNative, CommandNative,
			SingleOrNotFoundNative, AtLeastOneOrNotFoundNative,
			IntNative, EvalNative
		)
	val constants =
		List(
			"i" -> ComplexBigInt.i
		)
	
	def map = (natives map (n => (n.name -> n))) ++ constants toMap
}

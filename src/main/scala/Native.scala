package xyz.hyperreal.cras


object Builtins {
	def map =
		Map(
			"test" -> TestNative
		)
}

abstract class Native extends ((List[Any], Env) => Any) {
	def apply( args: List[Any], env: Env ): Any
}

object TestNative extends Native {
	def apply( args: List[Any], env: Env ) = 123
}
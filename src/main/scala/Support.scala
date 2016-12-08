package xyz.hyperreal.cras


object Support {
	def f0( env: Env ) = println( "wow" )
	
	def f1( a: Int, env: Env ) = println( a + 1 )
	
	def f2( a: Int, b: String, env: Env ) = (a + 1) + b
}

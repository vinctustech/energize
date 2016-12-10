package xyz.hyperreal.cras


object SupportFunctions {
	
	def print( env: Env, o: Any ) = println( o )
	
	def int( env: Env, v: String ) = v.toInt
	
	def str( env: Env, o: Any ) = o.toString
	
	def singleOrNotFound( env: Env, list: List[Any] ) =
		list.length match {
			case 0 => throw new CrasNotFoundException
			case 1 => list.head
			case _ => throw new CrasErrorException( "more than one item in list" )
		}
	
	def eval( env: Env, expr: String ) = evaluate( expr, env )
}
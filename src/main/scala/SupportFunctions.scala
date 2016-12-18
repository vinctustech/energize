package xyz.hyperreal.cras


object SupportFunctions {
	
	def print( env: Env, o: Any ) = println( o )
	
	def int( env: Env, v: String ) = v.toInt
	
	def long( env: Env, v: String ) = v.toLong
	
	def str( env: Env, o: Any ) = o.toString
	
	def singleOrNotFound( env: Env, list: List[Any] ) =
		list.length match {
			case 0 => throw new CrasNotFoundException
			case 1 => list.head
			case _ => throw new CrasErrorException( "more than one item in list" )
		}
	
	def atLeastOneOrNotFound( env: Env, count: Int ) =
		count match {
			case 0 => throw new CrasNotFoundException
			case _ => count
		}
		
	def eval( env: Env, expr: String ) = env.evaluate( expr )
}
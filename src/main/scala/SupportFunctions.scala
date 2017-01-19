package xyz.hyperreal.cras

import xyz.hyperreal.table.TextTable


object SupportFunctions {
	
	def print( env: Env, o: Any ) = println( o )
	
	def toInt( env: Env, v: String ) = v.toInt
	
	def toLong( env: Env, v: String ) = v.toLong
	
	def toString( env: Env, o: Any ) = o.toString
	
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
	
	def rndPrintable( env: Env, len: Int ) = List.fill( len )( util.Random.nextPrintableChar ).mkString
	
	def rndAlpha( env: Env, len: Int ) = List.fill( len )( (util.Random.nextInt('z' - 'a') + 'a').toChar ).mkString
	
	def rndInt( env: Env, low: Int, high: Int ) = util.Random.nextInt( high - low ) + low


	def show( env: Env, tab: Table ) = Console.print( TextTable(env.statement.executeQuery(s"select * from ${tab.name}")) )
}
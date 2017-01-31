package xyz.hyperreal.energize

import xyz.hyperreal.table.TextTable
import java.time.{Instant, ZoneOffset}


object UtilityFunctions {
	
	def print( env: Environment, o: Any ) = println( o )

	def toInt( env: Environment, v: String ) = v.toInt

	def toLong( env: Environment, v: String ) = v.toLong

	def toString( env: Environment, o: Any ) = o.toString

	def eval( env: Environment, expr: String ) = env.evaluate( expr )

	def rndPrintable( env: Environment, len: Int ) = List.fill( len )( util.Random.nextPrintableChar ).mkString

	def rndAlpha( env: Environment, len: Int ) = List.fill( len )( (util.Random.nextInt('z' - 'a') + 'a').toChar ).mkString

	def rndInt( env: Environment, low: Int, high: Int ) = util.Random.nextInt( high - low ) + low

	def show( env: Environment, tab: String ) = Console.print( TextTable(env.statement.executeQuery(s"select * from $tab")) )

	def Some( env: Environment, v: Any ) = scala.Some( v )

	def now( env: Environment ) = Instant.now.atOffset( ZoneOffset.UTC )

	def reject( env: Environment ): Unit = {
		throw new RejectRouteThrowable
	}
}
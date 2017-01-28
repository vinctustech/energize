package xyz.hyperreal.energize

import xyz.hyperreal.table.TextTable
import java.time.{Instant, ZoneOffset}


object SupportFunctions {
	
	def print( env: Env, o: Any ) = println( o )
	
	def toInt( env: Env, v: String ) = v.toInt
	
	def toLong( env: Env, v: String ) = v.toLong
	
	def toString( env: Env, o: Any ) = o.toString

	def eval( env: Env, expr: String ) = env.evaluate( expr )
	
	def rndPrintable( env: Env, len: Int ) = List.fill( len )( util.Random.nextPrintableChar ).mkString
	
	def rndAlpha( env: Env, len: Int ) = List.fill( len )( (util.Random.nextInt('z' - 'a') + 'a').toChar ).mkString
	
	def rndInt( env: Env, low: Int, high: Int ) = util.Random.nextInt( high - low ) + low

	def show( env: Env, tab: String ) = Console.print( TextTable(env.statement.executeQuery(s"select * from $tab")) )

	def Some( env: Env, v: Any ) = scala.Some( v )

	def now( env: Env ) = Instant.now.atOffset( ZoneOffset.UTC )
}
package xyz.hyperreal.energize

import java.time.{Instant, ZoneOffset}
import javax.script._
import jdk.nashorn.api.scripting._

import util.Random._

import xyz.hyperreal.table.TextTable


object UtilityFunctions {
	
	def print( env: Environment, o: Any ) = println( o )

	def toInt( env: Environment, v: String ) = v.toInt

	def toLong( env: Environment, v: String ) = v.toLong

	def toString( env: Environment, o: Any ) = o.toString

	def eval( env: Environment, expr: String ) = env.evaluate( expr )

	def rndPrintable( env: Environment, len: Int ) = new String( Array.fill( len )( util.Random.nextPrintableChar) )

	def rndAlpha( env: Environment, len: Int ) = new String( Array.fill( len )((util.Random.nextInt('z' - 'a') + 'a').toChar) )

	def rndInt( env: Environment, low: Int, high: Int ) = nextInt( high + 1 - low ) + low

	def show( env: Environment, tab: String ) = Console.print( TextTable(env.statement.executeQuery(s"select * from $tab")) )

	def Some( env: Environment, v: Any ) = scala.Some( v )

	def now( env: Environment ) = Instant.now.atOffset( ZoneOffset.UTC )

	def reject( env: Environment ) = throw new RejectRouteThrowable

	def jsrequest( env: Environment ): Unit = {
		for ((k, v) <- env.pathMap)
			env.jseng.put( k, v )

		env.jseng.put( "req", new RequestObject(env) )
	}

	def jscompile( env: Environment, code: String ) = {
		jsrequest( env )
		env.jseng.compile( code )
	}

	def jseval( env: Environment, code: String ) = {
		jscompile( env, code ).eval
	}
}

class RequestObject( env: Environment ) extends AbstractJSObject {
	override def getMember( name: String ) =
		name match {
			case "body" => env.sbindings( "entity" ).value
			case "query" =>
				new AbstractJSObject {
					override def getMember( name: String ) = env.queryMap get name
				}
		}
}

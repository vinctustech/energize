package xyz.hyperreal.energize2

import java.net.URI
import java.nio.charset.Charset
import java.sql.{Connection, SQLException, Statement}

import collection.JavaConverters._
import collection.mutable.{ArrayBuffer}
import org.apache.http.client.utils.URLEncodedUtils
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}
import xyz.hyperreal.bvm.{Compilation, VM}


class Processor( val code: Compilation, val connection: Connection, val statement: Statement, val db: Database,
								 val resources: Map[String, Resource], val key: String ) {

	private val router = code.functions( "_router_" )._1
	/*private [energize]*/ val vm = new VM( code, Array(), false, true, this )
	private val resourceMutex = new Object

	vm.execute

	def resource( name: String ) = resources( db.desensitize(name) )

	def renameResource( oldname: String, newname: String ) = resourceMutex.synchronized {

	}

	def renameField( resource: String, oldname: String, newname: String ) = resourceMutex.synchronized {

	}

	val miscellaneousRoutes = new ArrayBuffer[Route]

	def resourceLookup( name: String ) = resourceMutex.synchronized {
		resources get db.desensitize( name )
	}

	def call( name: String, args: List[Any] ) = {
		code.functions get name match {
			case None =>
				val callable =
					code.variables get name match {
						case None =>
							code.constants get name match {
								case None => sys.error( s"no function or callable defined or pre-defined named '$name'" )
								case Some( c ) => c
							}
						case Some( v ) => v
					}

				vm.call( callable, args )
			case Some( (f, _) ) =>
				vm.call( f, args )
		}
	}

	def result( name: String, args: List[Any] ) = call( name, args ).asInstanceOf[(Int, String, OBJ)]

	def process( method: String, uri: String, parms: Map[String, Any], body: AnyRef ) = resourceMutex.synchronized {
//		val res =
//			path.indexOf( '/', 1 ) match {
//				case -1 => path substring 1
//				case idx => path substring( 1, idx )
//			}

//		resourceLookup( res ) match {
//			case None =>
//				routeSearch( method, path, miscellaneousRoutes )
//			case Some( resource ) =>
//				routeSearch( method, path, resource.routes )
//		}

		def notfound = {
			val (sc, ctype, obj) = result( "NotFound", List("route not found") )

			(sc, ctype, DefaultJSONWriter.toString( obj ))
		}

		val requri = new URI( uri )
		val reqpath = requri.getPath
		val reqquery = Map( URLEncodedUtils.parse( requri, Charset.forName("UTF-8") ).asScala map (p => (p.getName, p.getValue)): _* )	//todo: configuration charset for url decoding?

		val (sc, mt, obj) =
			try {
				vm.call( router, List(method, reqpath, reqquery, parms, if (body eq null) null else DefaultJSONReader.fromString( body.toString )) ) match {//todo: handle non-string request body
					case "no match" => notfound
					case res => res
				}
			} catch {
				case e: UnauthorizedException =>
					result( "Unauthorized", List(s"""realm="${e.getMessage}"""") )
				case e: ExpiredException =>
					result( "Unauthorized", List(s"""realm="${e.getMessage}", error="invalid_token", error_description="The access token expired"""") )
				case e: ForbiddenException =>
					result( "Forbidden", List(e.getMessage) )
				case e: BadRequestException =>
					result( "BadRequest", List(e.getMessage) )
				case e: NotFoundException =>
					result( "NotFound", List(e.getMessage) )
				case e: SQLException if db.conflict( e.getMessage )  =>
					result( "Conflict", List(e.getMessage) )
			}

		val obj1 =
			if (obj.isInstanceOf[OBJ])
				DefaultJSONWriter.toString( obj.asInstanceOf[Map[String, Any]] )
			else
				obj

		(sc.asInstanceOf[Int], mt.asInstanceOf[String], obj1.asInstanceOf[AnyRef])
	}
}

class NotFoundException( error: String ) extends Exception( error )

class BadRequestException( error: String ) extends Exception( error )

class ForbiddenException( error: String ) extends Exception( error )

class UnauthorizedException( realm: String ) extends Exception( realm )

class ExpiredException( realm: String ) extends Exception( realm )

//	val EOP = '\uFFFF'
//
//	abstract class Type[T]( override val toString: String ) {
//		val valid: Char => Boolean
//
//		def result( input: String ): Option[T]
//	}
//
//	val STRING_TYPE =
//		new Type[String]( "string-type" ) {
//			val valid = (c: Char) => c != '/' && c != Processor.EOP
//
//			def result( input: String ): Option[String] = Some( input )
//		}
//	val INTEGER_TYPE =
//		new Type[Int]( "integer-type" ) {
//			val valid = '0' to '9' toSet
//
//			def result( input: String ): Option[Int] = Some( input.toInt )
//		}
//	val HEXINTEGER_TYPE =
//		new Type[Int]( "hexInteger-type" ) {
//			val valid = ('0' to '9' toSet) ++ ('A' to 'F') ++ ('a' to 'f')
//
//			def result( input: String ): Option[Int] = Some( Integer.parseInt(input, 16) )
//		}
//	val UUID_TYPE =
//		new Type[UUID]( "uuid-type" ) {
//			val valid = ('0' to '9' toSet) ++ ('A' to 'F') ++ ('a' to 'f') + '-'
//
//			def result( input: String ): Option[UUID] =
//				try {
//					Some( UUID.fromString(input) )
//				} catch {
//					case _: Exception => None
//				}
//		}
//	val typeMap =
//		Map(
//			"string" -> STRING_TYPE,
//			"integer" -> INTEGER_TYPE,
//			"hexInteger" -> HEXINTEGER_TYPE,
//			"uuid" -> UUID_TYPE
//		)
//
//	def pathMatch( pathPattern: Pattern, pathString: String ): Option[Map[String, Any]] = {
//		pathPattern.matches( pathString, anchored = true )// map {case (k, (_, _, v)) => (k, v.toString)}
//	}

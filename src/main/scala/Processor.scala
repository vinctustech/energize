package xyz.hyperreal.energize2

import java.net.URI
import java.nio.charset.Charset
import java.sql.{Connection, SQLException, Statement}

import collection.JavaConverters._
import collection.mutable.ArrayBuffer

import org.apache.http.client.utils.URLEncodedUtils

import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}
import xyz.hyperreal.bvm.{deref, Compilation, VM}

import scala.util.parsing.input.Position


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

	def result( name: String, res: Response, args: List[Any] ) = call( name, res :: args )

	//todo: need a router for each resource and a misc router
	def process(method: String, uri: String, reqheaders: MessageHeaders, body: AnyRef, resheaders: MessageHeaders ) = resourceMutex.synchronized {
		var resStatusCode = 200
		var resType = "application/octet-stream"
		var resTypeSet = false
    var resBody: AnyRef = ""

		val requri = new URI( uri )
		val reqpath = requri.getPath
		val reqquery = Map( URLEncodedUtils.parse( requri, Charset.forName("UTF-8") ).asScala map (p => (p.getName, p.getValue)): _* )	//todo: configuration charset for url decoding?
		val reqbody = if (body eq null) null else DefaultJSONReader.fromString( body.toString )
		val req =
			Map(
        "body" -> reqbody,
				"get" -> {
					(vm: VM, apos: Position, ps: List[Position], args: Any) =>
						deref( args ) match {
							case h: String => reqheaders( h )
							case _ => problem( apos, "error: expected one string argument" )
						}
					},
				"method" -> method,
				"path" -> reqpath,
        "query" -> reqquery,
        "url" -> uri
			)
		val res =
			new Response {
				def get( header: String ) = {
					resheaders( header )
					this
				}

				def set( header: String, value: String ) = {
					resheaders( header ) = value
					this
				}

				def status( code: Int ) = {
					resStatusCode = code
					this
				}

				def `type`( typ: String ) = {
					resType = typ
					resTypeSet = true
					this
				}

				def send( body: AnyRef ) = {
          resBody = body

          body match {
            case _: String =>
              if (!resTypeSet)
                resType = "text/html"
            case _: Array[_] | _: Seq[_] =>
              if (!resTypeSet)
                resType = "application/octet-stream"
            case _: OBJ =>
              if (!resTypeSet)
                resType = "application/json"

            resBody = DefaultJSONWriter.toString( body.asInstanceOf[Map[String, Any]] )
          }

					this
				}
			}

    //todo: handle non-string request body
    try {
      if (vm.call( router, List(method, reqpath, reqquery, reqbody, req, res) ) == 'nomatch)
        result( "NotFound", res, List("route not found") )
    } catch {
      case e: UnauthorizedException =>
        result( "Unauthorized", res, List(s"""realm="${e.getMessage}"""") )
      case e: ExpiredException =>
        result( "Unauthorized", res, List(s"""realm="${e.getMessage}", error="invalid_token", error_description="The access token expired"""") )
      case e: ForbiddenException =>
        result( "Forbidden", res, List(e.getMessage) )
      case e: BadRequestException =>
        result( "BadRequest", res, List(e.getMessage) )
      case e: NotFoundException =>
        result( "NotFound", res, List(e.getMessage) )
      case e: SQLException if db.conflict( e.getMessage )  =>
        result( "Conflict", res, List(e.getMessage) )
    }

		(resStatusCode, resType, resBody)
	}
}

class NotFoundException( error: String ) extends Exception( error )

class BadRequestException( error: String ) extends Exception( error )

class ForbiddenException( error: String ) extends Exception( error )

class UnauthorizedException( realm: String ) extends Exception( realm )

class ExpiredException( realm: String ) extends Exception( realm )

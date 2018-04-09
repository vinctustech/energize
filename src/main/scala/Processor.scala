//@
package xyz.hyperreal.energize

import java.lang.reflect.InvocationTargetException
import java.net.URI
import java.nio.charset.Charset
import java.sql.{Connection, SQLException, Statement}

import collection.JavaConverters._

import scala.util.parsing.input.Position

import org.apache.http.client.utils.URLEncodedUtils

import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}
import xyz.hyperreal.bvm.{Compilation, VM, deref, undefined}


object Processor {

	val charset = Charset forName SERVER.getString( "charset" )

}

class Processor( val code: Compilation, val connection: Connection, val statement: Statement, val db: Database,
								 val resources: Map[String, Resource], val routes: List[Route], val key: String, val users: Resource ) {

	val mutex = new Object

	private val router = code.functions( "_router_" )._1
	/*private [energize]*/ val vm = new VM( code, Array(), false, true, this )

	//vm.execute

	def discard: Unit = {
		connection.close
	}

	def resource( name: String ) = resources( db.desensitize(name) )

	def renameResource( oldname: String, newname: String ) = mutex.synchronized {

	}

	def renameField( resource: String, oldname: String, newname: String ) = mutex.synchronized {

	}

	def readMedia( id: Long ) = {
		val res = statement.executeQuery( s"SELECT * FROM _media_ WHERE $idIn = $id" )

		res.next

		val typ = res.getString( 2 )

		if (db == PostgresDatabase)
			(typ, res.getBytes( 3 ))
		else {
			val blob = res.getBlob( 3 )

			(typ, blob.getBytes( 0, blob.length.toInt ))
		}
	}

//	val miscellaneousRoutes = new ArrayBuffer[Route]	// implement route partitioning based on resource

	def resourceLookup( name: String ) = mutex.synchronized {
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

	def result( name: String, res: Response, error: String ) = call( name, List(res, error) )

	//todo: need a router for each resource and a misc router
	def process( method: String, uri: String, reqMessage: Message, body: AnyRef, resMessage: Message ) = mutex.synchronized {
//					println(method, uri, reqheaders, body, resheaders)//dbg
		var resStatusCode = 200
		var resType = "application/octet-stream"
		var resTypeSet = false
    var resBody: AnyRef = ""

		val requri = new URI( uri )
		val reqpath = requri.getPath
		val reqquery = Map( URLEncodedUtils.parse( requri, Processor.charset ).asScala map (p => (p.getName, p.getValue)): _* )	//todo: configuration charset for url decoding?
		val reqbody = if (body eq null) null else DefaultJSONReader.fromString( body.toString )
		val req =
			Map(
        "body" -> reqbody,
				"get" -> {
					(_: VM, apos: Position, _: List[Position], args: Any) =>
						deref( args ) match {
							case h: String => reqMessage( h )
							case _ => problem( apos, "error: expected one string argument" )
						}
					},
				"hostname" -> {
          if (reqMessage eq null)
            undefined
          else {
            val h = reqMessage("Host")

						if (h eq null)
							undefined
						else
							h indexOf ':' match {
								case -1 => h
								case idx => h.substring( 0, idx )
							}
          }
				},
				"parse" -> {
					(_: VM, apos: Position, _: List[Position], args: Any) =>
						deref( args ) match {
							case h: String => reqMessage.parse( h )
							case _ => problem( apos, "error: expected one string argument" )
						}
					},
				"method" -> method,
				"path" -> reqpath,
        "query" -> reqquery,
        "url" -> uri,
				"proc" -> this
			)
		val res =
			new Response {
				def get( header: String ) = {
					resMessage( header )
				}

				def set( header: String, value: String ) = {
					resMessage( header ) = value
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
						case null =>
              if (!resTypeSet)
                resType = "text/html"

							resBody = "null"
            case _: String =>
              if (!resTypeSet)
                resType = "text/html"
            case _: Array[_] | _: Seq[_] =>
              if (!resTypeSet)
                resType = "application/octet-stream"
            case _: Map[_, _] =>
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
        result( "NotFound", res, s"route not found: $method $uri" )
    } catch {
			case e: InvocationTargetException =>
				handleException( e.getCause, res )
			case e: Exception =>
//				e.printStackTrace //todo: handle server side exceptions better so that we can get the error info
				handleException( e, res )
		}

		(resStatusCode, resType, resBody)
	}

	def handleException( e: Throwable, res: Response ) =
		e match {
			case e: UnauthorizedException =>
				result( "Unauthorized", res, e.getMessage )
			case e: ForbiddenException =>
				result( "Forbidden", res, e.getMessage )
			case e: BadRequestException =>
				result( "BadRequest", res, e.getMessage )
			case e: NotFoundException =>
				result( "NotFound", res, e.getMessage )
			case e: SQLException if db.conflict( e.getMessage ) =>
				result( "Conflict", res, "unique constraint violation" )
			case e: Exception =>
				e.printStackTrace()
				result( "InternalServerError", res, e.getMessage )
		}

}

class NotFoundException( error: String ) extends Exception( error )

class BadRequestException( error: String ) extends Exception( error )

class ForbiddenException( error: String ) extends Exception( error )

class UnauthorizedException( realm: String ) extends Exception( realm )

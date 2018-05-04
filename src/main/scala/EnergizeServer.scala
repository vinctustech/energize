//@
package xyz.hyperreal.energize

import java.util.concurrent.TimeUnit
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.Charset

import org.apache.http.{HttpRequest, ExceptionLogger, HttpResponse, HttpEntityEnclosingRequest}
import org.apache.http.entity.ContentType
import org.apache.http.impl.nio.bootstrap.ServerBootstrap
import org.apache.http.impl.nio.reactor.IOReactorConfig
import org.apache.http.nio.entity.{NByteArrayEntity, NFileEntity, NStringEntity}
import org.apache.http.nio.protocol._
import org.apache.http.protocol.HttpContext
import org.apache.http.HttpStatus._


object EnergizeServer {
	def instance( config: io.Source, json: Boolean, port: Int ) = {
		val (connection, statement, db) = Definition.dbconnect
		val key = AUTHENTICATION.getString( "key" )

		sys.addShutdownHook {
			connection.close
		}

		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
    println( s"Energize server v$VERSION" )
		println( s"loading definition..." )

		val pro =
			if (json)
				Definition.defineFromJSON( config, connection, statement, db, key )
			else
				Definition.define( config, connection, statement, db, key )

		println( s"starting server on port $port" )

		val res = new EnergizeServer( pro, port )

		res.start
		println( s"server listening on port $port" )
		res
	}
}

class EnergizeServer( pro: Processor, port: Int ) {
	val origin = SERVER.getString( "origin" )
	val charset = Charset forName SERVER.getString( "charset" )
	val `text/html` = mktype( "text/html" )
	val `application/json` = mktype( "application/json" )
	val config = IOReactorConfig.custom
		.setSoTimeout( SERVER.getInt("timeout") )
		.setTcpNoDelay(true)
		.build
	val server = ServerBootstrap.bootstrap
		.setListenerPort( port )
		.setServerInfo( s"${SERVER.getString("name")}/$VERSION" )
		.setIOReactorConfig(config)
		.setSslContext(null)
		.setExceptionLogger(ExceptionLogger.STD_ERR)
		.registerHandler("*", new RequestHandler)
		.create

	private def mktype( typ: String ) =
		if (typ.startsWith( "text/" ) || (typ containsSlice "json"))
			ContentType.create( typ, charset )
		else
			ContentType.create( typ )

	def start {
		Runtime.getRuntime.addShutdownHook(new Thread {
			override def run {
				shutdown
			}
		})

		server.start
	}

	def shutdown: Unit = {
		server.shutdown(0, TimeUnit.MILLISECONDS)
	}

	class RequestHandler extends HttpAsyncRequestHandler[HttpRequest] {

		def processRequest (
				request: HttpRequest,
				context: HttpContext): HttpAsyncRequestConsumer[HttpRequest] = {
			new BasicAsyncRequestConsumer
		}

		def handle(
				request: HttpRequest,
				httpexchange: HttpAsyncExchange,
				context: HttpContext) {
			val response = httpexchange.getResponse

			handleInternal( request, response, context )
			httpexchange.submitResponse( new BasicAsyncResponseProducer(response) )
		}

		private def empty( length: Long, typ: String ) =
			new NByteArrayEntity( new Array[Byte]( 0 ), 0, 0, mktype(typ) ) {
				override def getContentLength = length
			}

		private def handleInternal(
				request: HttpRequest,
				response: HttpResponse,
				context: HttpContext) {

			val method = request.getRequestLine.getMethod.toUpperCase
			val target = request.getRequestLine.getUri
			val method1 =
				if (method == "HEAD")
					"GET"
				else
					method

			if (method1 == "OPTIONS") {
				response.setHeader( "Access-Control-Allow-Origin", origin )
				response.setHeader( "Access-Control-Allow-Headers", "Accept,Content-Type,Origin,Authorization")
				response.setHeader( "Access-Control-Allow-Methods", "HEAD,GET,PUT,POST,PATCH,DELETE,OPTIONS")
				response.setStatusCode( SC_OK )
				response.setEntity(
					new NStringEntity( s"<html><body><p>Supported methods: HEAD, GET, PUT, POST, PATCH, DELETE, OPTIONS</p></body></html>", `text/html` ) )
			} else {
				response.setHeader( "Access-Control-Allow-Origin", origin )

				try {
          val req = new HttpComponentsMessageWrapper( request )
          val res = new HttpComponentsMessageWrapper( response )
					val (status, ctype, contents) =
						request match {
							case withEntity: HttpEntityEnclosingRequest =>
								val buf = new ByteArrayOutputStream
								val entity = withEntity.getEntity

								entity.writeTo( buf )
								pro.process( method1, target, req, buf.toString, res )
							case _ =>
								pro.process( method1, target, req, null, res )
						}

					if (status == SC_NOT_FOUND && contents == null) {
						response.setStatusCode( SC_NOT_FOUND )

						val entity = new NStringEntity(
							s"<html><body><h1>$target not found</h1></body></html>", `text/html`)

						response.setEntity( entity )
					} else {
						response.setStatusCode( status )

						if (status == SC_UNAUTHORIZED) {
							response.setHeader( "WWW-Authenticate", s"Basic $contents" )
						} else if (contents ne null) {
							val entity =
								contents match {
									case s: String => new NStringEntity( s, mktype(ctype) )
									case a: Array[Byte] => new NByteArrayEntity( a, mktype(ctype) )
									case a: Seq[_] => new NByteArrayEntity( a.asInstanceOf[Seq[Byte]].toArray, mktype(ctype) )
									case file: File =>
										if (!file.exists || file.isDirectory) {
											response.setStatusCode( SC_NOT_FOUND )
											new NStringEntity( s"<html><body><h1>$target not found</h1></body></html>", `text/html` )
										} else if (!file.canRead) {
											response.setStatusCode( SC_FORBIDDEN )
											new NStringEntity( "<html><body><h1>Access denied</h1></body></html>", `text/html` )
										} else
											new NFileEntity( file, mktype(ctype) )
								}

							if (method == "HEAD") {
								response.setEntity( empty(entity.getContentLength, ctype) )
							} else
								response.setEntity( entity )
						}
					}
				} catch {
					case e: Exception =>
						e.printStackTrace( Console.err )
						response.setStatusCode( SC_INTERNAL_SERVER_ERROR )
						response.setEntity(
							new NStringEntity( s""""error": "${e.getMessage}"""", `application/json` ) )
				}
			}
		}
	}
}
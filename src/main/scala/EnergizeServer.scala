package xyz.hyperreal.energize2

import java.util.concurrent.TimeUnit
import java.io.{ByteArrayOutputStream, File}
import java.net.URLDecoder
import java.text.SimpleDateFormat
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

		val pro =
			if (json)
				Definition.defineFromJSON( config, connection, statement, db, key )
			else
				Definition.define( config, connection, statement, db, key )

		println( s"starting server on port $port" )

		val res = new EnergizeServer( pro, port )

		res.start
		res
	}
}

class EnergizeServer( pro: Processor, port: Int ) {
	val origin = SERVER.getString( "origin" )
	val docroot = SERVER.getString( "docroot" )
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
	val EXTENSION = ".*\\.(.*)"r
	val MODIFIED = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss")

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
		server.shutdown(1000, TimeUnit.MILLISECONDS)
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
          val reqheaders = new HttpComponentsMessageHeaders( request )
          val resheaders = new HttpComponentsMessageHeaders( response )
					val (status, ctype, contents) =
						request match {
							case withEntity: HttpEntityEnclosingRequest =>
								val buf = new ByteArrayOutputStream
								val entity = withEntity.getEntity

								entity.writeTo( buf )
								pro.process( method1, target, reqheaders, buf.toString, resheaders )
							case _ =>
								pro.process( method1, target, reqheaders, null, resheaders )
						}

					if (status == SC_NOT_FOUND) {
						val file = {
							val f = new File(docroot, URLDecoder.decode(target, "UTF-8"))

							if (f isDirectory) {
								val index = new File(f, "index.html")

								if (index.exists)
									index
								else
									f
							} else
								f
						}

						if (!file.exists) {
							response.setStatusCode( SC_NOT_FOUND )

							val entity = new NStringEntity(
								s"<html><body><h1>$target not found</h1></body></html>", `text/html`)

							response.setEntity( entity )
						} else if (!file.canRead) {
							response.setStatusCode( SC_FORBIDDEN )

							val entity = new NStringEntity(
								"<html><body><h1>Access denied</h1></body></html>", `text/html`)

							response.setEntity( entity )
						} else if (file.isDirectory) {
							val path =
								file.getAbsolutePath match {
									case p if p endsWith "/" => p dropRight 1
									case p => p
								}
							val parent =
								file.getParent match {
									case null => path
									case p => p
								}
							val files = file.listFiles.toList
							val index =
								<html>
									<head>
										<title>Index of
											{path}
										</title>
									</head>
									<body>
										<h1>Index of
											{path}
										</h1>
										<table>
											<tr>
												<th valign="top">
													<img src="/icons/blank.gif" alt="[ICO]"/>
												</th>
												<th>
													<a href="?C=N;O=D">Name</a>
												</th>
												<th>
													<a href="?C=M;O=A">Last modified</a>
												</th>
												<th>
													<a href="?C=S;O=A">Size</a>
												</th>
												<th>
													<a href="?C=D;O=A">Description</a>
												</th>
											</tr>
											<tr>
												<td valign="top">
													<img src="/icons/back.gif" alt="[PARENTDIR]"/>
												</td>
												<td>
													<a href={parent}>Parent Directory</a>
												</td> <td>
												&nbsp;
											</td>
												<td align="right">-</td>
												<td>
													&nbsp;
												</td>
											</tr>{files map (f =>
											<tr>
												<td valign="top">
													<img src={f.getName} alt="[DIR]"/>
												</td>
												<td>
													<a href={f.getName}>
														{f.getName}
													</a>
												</td>
												<td align="right">
													{MODIFIED.format(f.lastModified)}
												</td>
												<td align="right">-</td>
												<td>
													&nbsp;
												</td>
											</tr>)}
										</table>
									</body>
								</html>
							val entity = new NStringEntity("<!DOCTYPE html>\n" + index, `text/html`)

							response.setStatusCode(SC_OK)
							response.setEntity(entity)
						} else {
							val EXTENSION(ext) = file.getName.toLowerCase
							val contentType =
								ext match {
									case "html"|"htm" => `text/html`
									case "jpeg"|"jpg" => ContentType.create("image/jpeg")
									case "css" => ContentType.create("text/css")
									case "js" => ContentType.create("application/javascript")
									case "json" => `application/json`
								}

							response.setStatusCode(SC_OK)

							val body = new NFileEntity(file, contentType)

							response.setEntity(body)
						}
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
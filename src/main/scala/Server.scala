package xyz.hyperreal.energize

import java.util.concurrent.TimeUnit
import java.io.{ByteArrayOutputStream, File}
import java.net.URLDecoder
import java.text.SimpleDateFormat

import org.apache.http.{HttpRequest, ExceptionLogger, HttpResponse, HttpEntityEnclosingRequest}
import org.apache.http.entity.ContentType
import org.apache.http.impl.nio.bootstrap.ServerBootstrap
import org.apache.http.impl.nio.reactor.IOReactorConfig
import org.apache.http.nio.entity.{NByteArrayEntity, NFileEntity, NStringEntity}
import org.apache.http.nio.protocol._
import org.apache.http.protocol.HttpContext
import org.apache.http.HttpStatus._


class Server( env: Environment ) {
	val origin = SERVER.getString( "origin" )
	val config = IOReactorConfig.custom
		.setSoTimeout( SERVER.getInt("timeout") )
		.setTcpNoDelay(true)
		.build
	val server = ServerBootstrap.bootstrap
		.setListenerPort( SERVER.getInt("port") )
		.setServerInfo( SERVER.getString("name") + "/" + SERVER.getString("version") )
		.setIOReactorConfig(config)
		.setSslContext(null)
		.setExceptionLogger(ExceptionLogger.STD_ERR)
		.registerHandler("*", new RequestHandler)
		.create
	val docroot = SERVER.getString( "docroot" )
	val EXTENSION = ".*\\.(.*)"r
	val MODIFIED = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss")

	def start {
		server.start
		server.awaitTermination(Long.MaxValue, TimeUnit.DAYS)

		Runtime.getRuntime.addShutdownHook(new Thread {
			override def run {
				server.shutdown(1, TimeUnit.MILLISECONDS)
			}
		})
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

		private def empty( length: Long ) =
			new NByteArrayEntity( new Array[Byte]( 0 ), 0, 0, ContentType.APPLICATION_JSON ) {
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
					new NStringEntity( s"<html><body><p>Supported methods: HEAD, GET, PUT, POST, PATCH, DELETE, OPTIONS</p></body></html>", ContentType.TEXT_HTML ) )
			} else {
				response.setHeader( "Access-Control-Allow-Origin", origin )

				try {
					val env1 =
						request.getFirstHeader( "Authorization" ) match {
							case null => env
							case h =>
								h.getValue match {
									case v if v startsWith "Basic " => env add "$Basic" -> v.substring( 6 )
									case _ => env
								}
						}
					val (status, contents) =
						request match {
							case withEntity: HttpEntityEnclosingRequest =>
								val buf = new ByteArrayOutputStream
								val entity = withEntity.getEntity

								entity.writeTo( buf )
								env1.process( method1, target, buf.toString )
							case _ =>
								env1.process( method1, target, null )
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
							response.setStatusCode(SC_NOT_FOUND)

							val entity = new NStringEntity(
								"<html><body><h1>File " + file.getPath +
									" not found</h1></body></html>",
								ContentType.create("text/html", "UTF-8"))

							response.setEntity(entity)
							println("File " + file.getPath + " not found")
						} else if (!file.canRead) {
							response.setStatusCode(SC_FORBIDDEN)

							val entity = new NStringEntity(
								"<html><body><h1>Access denied</h1></body></html>",
								ContentType.create("text/html", "UTF-8"))

							response.setEntity(entity)
							println("Cannot read file " + file.getPath)
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
							val entity = new NStringEntity("<!DOCTYPE html>\n" + index, ContentType.create("text/html", "UTF-8"))

							response.setStatusCode(SC_OK)
							response.setEntity(entity)
						} else {
							val EXTENSION(ext) = file.getName.toLowerCase
							val contentType =
								ext match {
									case "html" | "htm" => ContentType.create("text/html")
									case "jpeg" | "jpg" => ContentType.create("image/jpeg")
									case "css" => ContentType.create("text/css")
									case "js" => ContentType.create("application/javascript")
									case "json" => ContentType.create("application/json")
								}

							response.setStatusCode(SC_OK)

							val body = new NFileEntity(file, contentType)

							response.setEntity(body)
						}
					} else {
						response.setStatusCode( status )

						if (status == SC_UNAUTHORIZED)
							response.setHeader( "WWW-Authenticate", s"""Basic realm="$contents"""" )
						else if (contents ne null) {
							val entity = new NStringEntity( contents, ContentType.APPLICATION_JSON )

							if (method == "HEAD") {
								response.setEntity( empty(entity.getContentLength) )
							} else
								response.setEntity( entity )
						}
					}
				} catch {
					case e: Exception =>
						e.printStackTrace( Console.err )
						response.setStatusCode( SC_INTERNAL_SERVER_ERROR )
						response.setEntity(
							new NStringEntity( s"""{"error": "${e.getMessage}"}"""", ContentType.APPLICATION_JSON ) )
				}
			}
		}
	}
}
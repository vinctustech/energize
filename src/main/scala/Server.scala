package xyz.hyperreal.cras

import java.sql._
import java.util.concurrent.TimeUnit
import java.io.ByteArrayOutputStream

import org.apache.http.{MethodNotSupportedException, HttpStatus, HttpResponse, ExceptionLogger, HttpConnection, HttpException, HttpRequest, HttpEntityEnclosingRequest}
import org.apache.http.entity.ContentType
import org.apache.http.impl.nio.bootstrap.{HttpServer, ServerBootstrap}
import org.apache.http.impl.nio.reactor.IOReactorConfig
import org.apache.http.nio.entity.{NFileEntity, NStringEntity}
import org.apache.http.nio.protocol.{BasicAsyncRequestConsumer, BasicAsyncResponseProducer, HttpAsyncExchange, HttpAsyncRequestConsumer, HttpAsyncRequestHandler}
import org.apache.http.protocol.{HttpContext, HttpCoreContext}
import org.apache.http.ssl.SSLContexts


class Server( port: Int, env: Env ) {
	val config = IOReactorConfig.custom
		.setSoTimeout(15000)
		.setTcpNoDelay(true)
		.build

	val server = ServerBootstrap.bootstrap
		.setListenerPort(port)
		.setServerInfo("CRAS/0.1")
		.setIOReactorConfig(config)
		.setSslContext(null)
		.setExceptionLogger(ExceptionLogger.STD_ERR)
		.registerHandler("*", new RequestHandler)
		.create

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

		private def handleInternal(
				request: HttpRequest,
				response: HttpResponse,
				context: HttpContext) {

			val method = request.getRequestLine.getMethod.toUpperCase
			
			if (method == "OPTION" || method == "HEAD")
					throw new MethodNotSupportedException(method + " method not supported")

			val target = request.getRequestLine.getUri
			
			try {
				val data =
					request match {
						case withEntity: HttpEntityEnclosingRequest =>
							val buf = new ByteArrayOutputStream
							val entity = withEntity.getEntity
								
							entity.writeTo( buf )
							process( method, target, buf.toString, env )
						case noEntity =>
							process( method, target, null, env )
					}
					
				data match {
					case Some( d ) =>
						response.setStatusCode( HttpStatus.SC_OK )
						response.setEntity( new NStringEntity(d, ContentType.APPLICATION_JSON) )
					case None => 
						response.setStatusCode( HttpStatus.SC_NOT_FOUND )
						response.setEntity( new NStringEntity("<html><body><h1>404: Not Found</h1></body></html>", ContentType.TEXT_HTML) )
				}
			} catch {
				case e: Exception =>
					response.setStatusCode( HttpStatus.SC_INTERNAL_SERVER_ERROR )
					response.setEntity(
						new NStringEntity(s"<html><body><h1>500: Internal Server Error</h1><p>${e.getMessage}</p></body></html>", ContentType.TEXT_HTML) )
			}
		}
	}
}
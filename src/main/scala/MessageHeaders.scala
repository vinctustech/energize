//@
package xyz.hyperreal.energize2

import org.apache.http.{Header, HttpMessage}


trait MessageHeaders {

	def apply( header: String ): String

	def parse( header: String ): (String, Map[String, (String, Map[String, String])])

	def update( header: String, value: String )

}

object MessageHeaders {

	def elements( h: Header ) = h.getElements map (e => e.getName -> (e.getValue, e.getParameters map (p => p.getName -> p.getValue) toMap)) toMap

}

class HttpComponentsMessageHeaders( message: HttpMessage ) extends MessageHeaders {

	def apply( header: String ) =
		message.getFirstHeader( header ) match {
			case null => null
			case h => h.getValue
		}

	def parse( header: String ): (String, Map[String, (String, Map[String, String])]) = {
		message.getFirstHeader( header ) match {
			case null => null
			case h =>
				(h.getValue, MessageHeaders.elements( h ))
		}
	}

	def update( header: String, value: String ) = message.setHeader( header, value )

}

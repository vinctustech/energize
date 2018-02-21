//@
package xyz.hyperreal.energize2

import org.apache.http.HttpMessage


trait MessageHeaders {

	def apply( header: String ): String

	def parse( header: String ): (String, Map[String, (String, Map[String, String])])

	def update( header: String, value: String )

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
				(h.getValue, h.getElements map (e => (e.getName -> (e.getValue, e.getParameters map (p => (p.getName -> p.getValue)) toMap))) toMap)
		}
	}

	def update( header: String, value: String ) = message.setHeader( header, value )

}

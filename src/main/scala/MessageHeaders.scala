//@
package xyz.hyperreal.energize2

import org.apache.http.HttpMessage
import xyz.hyperreal.bvm.undefined


trait MessageHeaders {

	def apply( header: String ): String

//	def parse( header: String ): (String, List[(String, String)])

	def update( header: String, value: String )

}

class HttpComponentsMessageHeaders( message: HttpMessage ) extends MessageHeaders {

	def apply( header: String ) =
		message.getFirstHeader( header ) match {
			case null => null
			case h => h.getValue
		}

	def update( header: String, value: String ) = message.setHeader( header, value )

}

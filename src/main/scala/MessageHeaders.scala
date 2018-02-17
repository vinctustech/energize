//@
package xyz.hyperreal.energize2

import org.apache.http.HttpMessage
import xyz.hyperreal.bvm.undefined


class MessageHeaders( message: HttpMessage ) {

	def apply( header: String ) =
		message.getFirstHeader( header ) match {
			case null => undefined
			case h => h
		}

	def update( header: String, value: String ) = message.setHeader( header, value )

}

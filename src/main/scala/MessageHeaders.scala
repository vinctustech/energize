//@
package xyz.hyperreal.energize2

import org.apache.http.HttpMessage
import xyz.hyperreal.bvm.undefined


class MessageHeaders( message: HttpMessage ) {

	def apply( header: String ) =
		get( header ) match {
			case null => undefined
			case h => h.getValue
		}
	
	def get( header: String ) = message.getFirstHeader( header )

	def update( header: String, value: String ) = message.setHeader( header, value )

}

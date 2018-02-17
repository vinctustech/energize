//@
package xyz.hyperreal.energize2

import org.apache.http.HttpResponse
import xyz.hyperreal.bvm.undefined


class ResponseHeaders( response: HttpResponse ) {

	def apply( header: String ) =
		response.getFirstHeader( header ) match {
			case null => undefined
			case h => h
		}

	def update( header: String, value: String ) = response.setHeader( header, value )

}

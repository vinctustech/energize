//@
package xyz.hyperreal.energize

import org.apache.http.{Header, HeaderElement, HttpMessage}
import org.apache.http.message.BasicHeaderValueParser

import scala.collection.mutable


trait Message {

	def apply( header: String ): String

	def parse( header: String ): Map[String, (String, Map[String, String])]

	def update( header: String, value: String )

}

object Message {

	def elements( h: Header ): Map[String, (String, Map[String, String])] = elements( h.getElements )

	def elements( elems: Array[HeaderElement] ) = elems map (e => e.getName -> (e.getValue, e.getParameters map (p => p.getName -> p.getValue) toMap)) toMap

}

class SimpleMessage( headers: (String, String)* ) extends Message {

	private val map = mutable.HashMap( headers: _* )

	def apply( header: String ) = map get header orNull

	def parse( header: String ): Map[String, (String, Map[String, String])] =
		Message.elements( BasicHeaderValueParser.parseElements(apply(header), BasicHeaderValueParser.INSTANCE) )

	def update( header: String, value: String ) = map.update( header, value )

}

class HttpComponentsMessageWrapper( message: HttpMessage ) extends Message {

	def apply( header: String ) =
		message.getFirstHeader( header ) match {
			case null => null
			case h => h.getValue
		}

	def parse( header: String ): Map[String, (String, Map[String, String])] =
		message.getFirstHeader( header ) match {
			case null => null
			case h => Message.elements( h )
		}

	def update( header: String, value: String ) = message.setHeader( header, value )

}

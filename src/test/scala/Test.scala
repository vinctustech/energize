package xyz.hyperreal.energize2

import java.io.{PrintStream, ByteArrayOutputStream}


object Test {

  def dbconnect = Definition.dbconnect( "H2", "org.h2.Driver", "jdbc:h2:mem:", "sa", "" )

	def capture( code: String ) = {
		val buf = new ByteArrayOutputStream
		val (c, s, d) = dbconnect
		val key = AUTHENTICATION.getString( "key" )

		Console.withOut( new PrintStream(buf) )( Definition.define(io.Source.fromString(code), c, s, d, key) )
		c.close
		buf.toString.trim
	}
	
	def captureReturn( code: String ) = {
		val buf = new ByteArrayOutputStream
		val (c, s, d) = dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val ret = Console.withOut( new PrintStream(buf) )( Definition.define(io.Source.fromString(code), c, s, d, key) )

		c.close
		(ret, buf.toString.trim)
	}
}
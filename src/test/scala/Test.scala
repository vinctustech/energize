package xyz.hyperreal.energize

import java.io.{ByteArrayOutputStream, PrintStream}


object Test {

  def dbconnect = {
		val res@(c, s, d) = Definition.dbconnect

//		dropPostgresTables( c, s, d )
		res
	}

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
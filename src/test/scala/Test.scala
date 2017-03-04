package xyz.hyperreal.energize

import java.io.{PrintStream, ByteArrayOutputStream}


object Test {

  def dbconnect = Energize.dbconnect( "H2", "org.h2.Driver", "jdbc:h2:mem:", "sa", "" )

	def capture( code: String ) = {
		val buf = new ByteArrayOutputStream
		val (c, s, d) = dbconnect

		Console.withOut( new PrintStream(buf) )( Energize.configure(io.Source.fromString(code), c, s, d) )
		c.close
		buf.toString.trim
	}
	
	def captureReturn( code: String ) = {
		val buf = new ByteArrayOutputStream
		val (c, s, d) = dbconnect
		val ret = Console.withOut( new PrintStream(buf) )( Energize.configure(io.Source.fromString(code), c, s, d) )

		c.close
		(ret, buf.toString.trim)
	}
}
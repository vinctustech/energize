package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	def test {
		val (c, s) = dbconnect( "test", true )
		val env = configure( io.Source.fromFile("users.cras"), c, s )
		
		println( process("GET", "/users", null, env) )
		
		c.close
	}
	
	test
}
package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	def test {
		val (c, s) = dbconnect( "test", true )
		val env = configure( io.Source.fromFile("users.cras"), c, s )
		
		println( env.tables )
//		println( process("GET", "/eval", """ {"expr": "(i + 2)/2*i"} """, env) )
		
		c.close
	}
	
	test
}
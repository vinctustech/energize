package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	def test {
		val (c, s) = dbconnect( "test", true )
		val (tables, routes) = configuration( io.Source.fromFile("t0.info"), c )

		println( process("GET", "/api/v1/toDos", "", tables, routes, s) )
		
		c.close
	}
	
	test
	test
}
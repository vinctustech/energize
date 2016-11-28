package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	def test {
		connect( "test", true )

		val (tables, routes) = Interpreter( new File("t0.info") )

		println( process("GET", "/api/v1/toDos", "", tables, routes) )
		
		close
	}
	
	test
	test
}
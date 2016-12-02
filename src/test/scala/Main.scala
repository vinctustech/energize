package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	def test {
		val (c, s) = dbconnect( "test", true )
		val env = configure( io.Source.fromFile("todo.cras"), c, s )
		
		println( process("GET", "/api/v1/todo", "{}", env) )
		
		c.close
	}
	
	test
	test
}
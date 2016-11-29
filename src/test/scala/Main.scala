package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	def test {
		val (c, s) = dbconnect( "test", true )
		val (tables, routes) = configuration( io.Source.fromFile("todo.cras"), c )

		println( process("GET", "/api/v1/todo", "", tables, routes, s) )
		
		c.close
	}
	
	test
	test
}
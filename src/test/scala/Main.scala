package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	val (c, s) = dbconnect( "books", true )
	val env = configure( io.Source.fromFile("books.cras"), c, s )
	
	println( env.process("GET", "/books?filter=title:The+Adventures+of+Huckleberry+Finn", null) )
	println( env.process("GET", "/books/1", null) )
	
	c.close
}
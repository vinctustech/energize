package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	val (c, s) = dbconnect( "books", true )
	val env = configure( io.Source.fromFile("books.cras"), c, s )
	
//	println( env.process("PATCH", "/books/1", """ {"role": "super"} """) )
	println( env.process("GET", "/books", null) )
	println( env.process("GET", "/books/1", null) )
	
	c.close
}
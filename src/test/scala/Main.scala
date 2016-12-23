package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	val (c, s) = dbconnect( "books", true )
	val env = configure( io.Source.fromFile("books.cras"), c, s )
	
//  	println( env.process("GET", "/books?filter=title=The+Adventures+of+Huckleberry+Finn,books.id=3", null) )
 	println( env.process("GET", "/books?order=title:asc", null) )
	println( env.process("GET", "/books/1", null) )
	
	c.close
}
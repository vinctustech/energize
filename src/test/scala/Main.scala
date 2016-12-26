package xyz.hyperreal.cras

import java.io.File


object Main extends App {

// 	val (c, s) = dbconnect( "books", true )
// 	val env = configure( io.Source.fromFile("books.cras"), c, s )
// 	
// //  	println( env.process("GET", "/books?filter=title=The+Adventures+of+Huckleberry+Finn,books.id=3", null) )
//  	println( env.process("GET", "/books?order=title:asc", null) )
// 	println( env.process("GET", "/books/1", null) )

// 	val (c, s) = dbconnect( "customers", true )
// 	val env = configure( io.Source.fromFile("customers.cras"), c, s )
// 	
// //  	println( env.process("GET", "/customers?order=City:asc,PostalCode:desc", null) )
//  	println( env.process("GET", "/customers?filter=CustomerName~A%25,City=Berlin", null) )

// 	val (c, s) = dbconnect( "data", true )
// 	val env = configure( io.Source.fromFile("data.cras"), c, s )
// 	
// //  	println( env.process("GET", "/customers?order=City:asc,PostalCode:desc", null) )
// //   	println( env.process("GET", "/data?fields=first_name,city;limit=5;order=city:asc;filter=first_name~C%25", null) )
//   	println( env.process("GET", "/data?fields=id,first_name;limit=3;page=2", null) )

	val (c, s) = dbconnect( "test", true )
	val env = configure( io.Source.fromFile("test.cras"), c, s )
	
	c.close
}
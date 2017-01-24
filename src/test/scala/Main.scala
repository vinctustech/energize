package xyz.hyperreal.energize


object Main extends App {

 	val (c, s) = Test.dbconnect

// 	val env = Energize.configure( io.Source.fromFile("books.energize"), c, s )

//  	println( env.process("GET", "/books?filter=title=The+Adventures+of+Huckleberry+Finn,books.id=3", null) )
//	println( env.process("GET", "/books?order=title:asc", null) )
// 	println( env.process("GET", "/books/1", null) )

// 	val env = Energize.configure( io.Source.fromFile("/home/ed/projects/energize/examples/customers.energize"), c, s )

 //  	println( env.process("GET", "/customers?order=City:asc,PostalCode:desc", null) )
//	println( env.process("GET", "/customers?filter=CustomerName~A%25,City=Berlin", null) )

// 	val env = Energize.configure( io.Source.fromFile("/home/ed/projects/energize/examples/data.energize"), c, s )

//  	println( env.process("GET", "/customers?order=City:asc,PostalCode:desc", null) )
//   	println( env.process("GET", "/data?fields=first_name,city;limit=5;order=city:asc;filter=first_name~C%25", null) )
//   	println( env.process("GET", "/data?fields=id,first_name;limit=3;page=2", null) )

//	val env = Energize.configure( io.Source.fromFile("/home/ed/projects/energize/examples/t1.energize"), c, s )

//	val env = Energize.configure( io.Source.fromFile("/home/ed/projects/energize/examples/students.energize"), c, s )

//	println( env.process("POST", "/students", """{"name": "asdf", "classrooms": ["101", "307"]}""") )
//	println( env.process("GET", "/students/5", null) )
//	println( env.process("PUT", "/students/5", """{"name": "zxvc", "classrooms": ["105", "302"]}""") )
//	println( env.process("PUT", "/students/1", """{"name": "zxvc"}""") )
//	println( env.process("DELETE", "/students/5/classrooms/5", null) )
//	println( env.process("GET", "/students/1", null) )

	val env = Energize.configure( io.Source.fromFile("/home/ed/projects/energize/examples/todo.energize"), c, s )

	println( env.process("POST", "/todo", """{"name": "asdf", "status": 1}""") )
	c.close
}
package xyz.hyperreal.energize


object Main extends App {

	val (c, s, d) = Test.dbconnect//Energize.dbconnect

//	val code = parseExpression( "1 + 2" )
//	val env = Energize.configure( io.Source.fromString(""), c, s, d )
//
//	println( env.deref(code) )

// 	val env = Energize.configure( io.Source.fromFile("examples/customers.energize"), c, s )
//
//  println( env.process("GET", "/customers?order=City:asc,PostalCode:desc", null) )
//	println( env.process("GET", "/customers?filter=CustomerName~A%25,City=Berlin", null) )
//	println( env.process("GET", "/customers?order=City:asc,PostalCode:desc", null) )

//	val env = Energize.configure( io.Source.fromFile("examples/t1.energize"), c, s )

//	val env = Energize.configure( io.Source.fromFile("examples/students.energize"), c, s, d, "123" )
//
//	println( env.process("POST", "/students", """{"name": "carlos", "classrooms": ["101", "307"]}""") )
//	println( env.process("GET", "/students/5", null) )
//	println( env.process("PUT", "/students/5", """{"name": "zxvc", "classrooms": ["105", "302"]}""") )
//	println( env.process("PUT", "/students/1", """{"name": "zxvc"}""") )
//	println( env.process("DELETE", "/students/5/classrooms/5", null) )
//	println( env.process("GET", "/students/1", null) )
//	println( env.process("GET", "/students", null) )

//	val env = Energize.configure( io.Source.fromFile("examples/test.energize"), c, s )

//	val env = Energize.configure( io.Source.fromFile("examples/ts.energize"), c, s, d )
//
//	println( env.process("GET", "/ts", null) )

//val env = Energize.configure( io.Source.fromFile("examples/energize.energize"), c, s, d )
//
//	println( env.process("GET", "/apis/1?fields=name", null) )

//	val env = Energize.configure( io.Source.fromFile("examples/energize.energize"), c, s, d )
//
//	println( env.process("GET", "/apis/1?fields=name", null) )

//	val env = Energize.configure( io.Source.fromFile("examples/a.energize"), c, s, d )
//
//	println( env.process("GET", "/r", null) )

	c.close
}
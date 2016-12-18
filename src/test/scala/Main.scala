package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	val (c, s) = dbconnect( "test", true )
	val env = configure( io.Source.fromFile("test.cras"), c, s )
	
	println( env.process("PATCH", "/users/1", """ {"role": "super"} """) )
	println( env.process("GET", "/users", null) )
// 		println( env.process("GET", "/roles", null) )
	
	c.close
}
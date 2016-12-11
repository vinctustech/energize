package xyz.hyperreal.cras

import java.io.File


object Main extends App {

	val (c, s) = dbconnect( "test", true )
	val env = configure( io.Source.fromFile("test.cras"), c, s )
	
//	println( process("GET", "/test", null, env) )
// 		println( process("GET", "/users", null, env) )
// 		println( process("GET", "/roles", null, env) )
	
	c.close
}
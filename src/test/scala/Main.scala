package xyz.hyperreal.informatio

import java.io.File


object Main extends App {
	
	val (tables, routes) = Interpreter( new File("t0.info") )
			
	println( find("GET", "/api/v1/users/1", routes) )
	println( find("GET", "/api/v1/users", routes) )

}
package xyz.hyperreal.informatio

import java.io.File

import xyz.hyperreal.json.DefaultJSONReader


object Main extends App {
	
	val (tables, routes) = Interpreter( new File("t0.info") )

	println( process("GET", "/api/v1/users", """{"a": 123}""", tables, routes) )

	def process( reqmethod: String, reqpath: String, reqjson: String, tables: Map[String, (List[String], Map[String, Column])], routes: List[Route] ) {
		val json = DefaultJSONReader.fromString( reqjson )
		
		println( json )
	}
}
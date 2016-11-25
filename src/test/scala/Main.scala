package xyz.hyperreal.informatio

import java.io.File

import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}
import xyz.hyperreal.table._


object Main extends App {
	
	connect( "projects/informatio/test" )

	val (tables, routes) = Interpreter( new File("t0.info") )

	DefaultJSONWriter.write( process("GET", "/api/v1/users", "", tables, routes) )
		
	close
}
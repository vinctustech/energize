package xyz.hyperreal.informatio

import java.io.File

import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


object Main extends App {
	
	val (tables, routes) = Interpreter( new File("t0.info") )

	DefaultJSONWriter.write( process("GET", "/api/v1/users", """{"a": 123}""", tables, routes) )

	def process( reqmethod: String, reqpath: String, reqjson: String, tables: Map[String, (List[String], Map[String, Column])], routes: List[Route] ): Map[String, Any] = {
		val json = DefaultJSONReader.fromString( reqjson )
		val (vars, expr) =
			find( reqmethod, reqpath, routes ) match {
				case None => return DefaultJSONReader.fromString( """{"error": "bad route"}""" )
				case Some( ve ) => ve
			}
		
		println( eval( expr, vars, tables ) )
		DefaultJSONReader.fromString( """{"status": "ok"}""" )
	}
	
	def eval( expr: ExpressionAST, vars: Map[String, Any], tables: Map[String, (List[String], Map[String, Column])] ) =
		expr match {
			case FunctionExpression( "query", List(StringExpression(sql)) ) =>
				val res = statement.executeQuery( sql )
				
			case _ => sys.error( "error evaluating expression" )
		}
		
	close
}
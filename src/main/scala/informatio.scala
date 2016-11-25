package xyz.hyperreal

import java.sql._

import collection.mutable.HashMap

import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


package object informatio {
	
	var connection: Connection = null
	var statement: Statement = null
		
	sys.addShutdownHook {
		close
	}
	
	def connect( dbfile: String ) = {
		if (connection eq null)
			close

		Class.forName( "org.h2.Driver" )
		connection = DriverManager.getConnection( "jdbc:h2:~/" + dbfile, "sa", "" )
		statement = connection.createStatement
		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
		connection
	}
	
	def close {
		if (connection ne null)
			connection.close
	}

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
	
	def query( sql: String ) = {
		
	}
	
	def find( method: String, path: String, routes: List[Route] ): Option[(Map[String,String], ExpressionAST)] = {
		val segments = {
			val trimed = path.trim
			val l = trimed split "/" toList
			
			if (l == List( "" ))
				return None
			else if (l == List() && trimed.length > 1)
				return None
			else if (l.head != "")
				return None
			else
				if (l == List())
					l
				else
					l.tail
		}
		val len = segments.length
		val vars = new HashMap[String, String]
		
		for (Route( rmethod, uri, action) <- routes) {
			vars.clear
			
			if (len == uri.length && method == rmethod && uri.zip( segments ).forall {
				case (NameURISegment( route ), request) => route == request
				case (ParameterURISegment( name ), request) =>
					vars(name) = request
					true
				case _ => sys.error( "unknown segment type" )
			})
				return Some( (vars.toMap, action) )
		}
		
		None
	}
	
}
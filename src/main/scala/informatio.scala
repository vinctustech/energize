package xyz.hyperreal

import java.sql._

import collection.mutable.HashMap


package object informatio {
	
	var connection: Connection = null
	var statement: Statement = null
		
	sys.addShutdownHook {
		close
	}
	
	def connect = {
		if (connection eq null) {
			Class.forName( "org.h2.Driver" )
			connection = DriverManager.getConnection( "jdbc:h2:~/projects/informatio/test", "sa", "" )
			statement = connection.createStatement
		}
		
		connection
	}
	
	def close {
		if (connection ne null)
			connection.close
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
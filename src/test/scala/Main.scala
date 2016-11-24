package xyz.hyperreal.informatio

import java.io.File

import collection.mutable.HashMap


object Main extends App {
	
	val routes = Interpreter( new File("t0.info") )
		
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
	
	println( find("GET", "/api/v1/users/1", routes) )
	println( find("GET", "/api/v1/users", routes) )

}
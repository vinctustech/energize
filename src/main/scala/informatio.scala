package xyz.hyperreal

import java.sql._

import collection.mutable.{ListBuffer, HashMap}
import util.matching.Regex

import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


package object informatio {
	
	var connection: Connection = null
	var statement: Statement = null
	private val varRegex = "\\$([a-zA-Z][a-zA-Z0-9]*)".r
		
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

	def process( reqmethod: String, reqpath: String, reqbody: String, tables: Map[String, Table], routes: List[Route] ): String = {
		val (vars, expr) =
			find( reqmethod, reqpath, routes ) match {
				case None => return """{"status": "error", "reason": "bad route"}"""
				case Some( ve ) => ve
			}
		
		DefaultJSONWriter.toString( evalj( expr, vars, tables, reqbody ) )
	}
	
	def eval( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String ): Any =
		expr match {
			case FunctionExpression( "insert", List(table, json) ) =>
				val t = eval( table, vars, tables, reqbody ).asInstanceOf[Table]
				val j = evalj( json, vars, tables, reqbody )
				val com = new StringBuilder( "insert into " )
				val last = t.names.last
				
				com ++= t.name
				com ++= t.names.mkString( "(", ", ", ") values(" )
				
				for (c <- t.names) {
					j get c match {
						case None => com ++= "null"
						case Some( v ) =>
							if (t.columns(c).typ == StringType) {
								com += '\''
								com ++= String.valueOf( v )
								com += '\''
							} else
								com ++= String.valueOf( v )
					}
					
					if (c != last)
						com ++= ", "
				}
				
				com += ')'
				
				Map(
					"status" -> "ok",
					"update" -> statement.executeUpdate( com.toString )
				)
			case FunctionExpression( "update", List(table, json, id) ) =>
				val t = eval( table, vars, tables, reqbody ).asInstanceOf[Table]
				val j = evalj( json, vars, tables, reqbody )
				val idv = evali( id, vars, tables, reqbody )
				val com = new StringBuilder( "update " )
				val last = t.names.last
				
				com ++= t.name
				com ++= " set "
				com ++=
					j.toList map {
						case (k, v) if t.columns(k).typ == StringType => k + "='" + String.valueOf( v ) + "'"
						case (k, v) => k + "=" + String.valueOf( v )
					} mkString ", "
				com ++= " where id="
				com ++= idv.toString
				
				Map(
					"status" -> "ok",
					"update" -> statement.executeUpdate( com.toString )
				)
			case FunctionExpression( "command", List(sql) ) =>
				val com = evals( sql, vars, tables, reqbody )
				
				Map(
					"status" -> "ok",
					"update" -> statement.executeUpdate( com.toString )
				)
			case FunctionExpression( "query", List(sql) ) =>
				val com = evals( sql, vars, tables, reqbody )
				val res = statement.executeQuery( com )
				val list = new ListBuffer[Map[String, Any]]
				val md = res.getMetaData
				val count = md.getColumnCount
				
				while (res.next)
					list += Map( (for (i <- 1 to count) yield (md.getColumnName(i), res.getObject(i))): _* )
				
				Map(
					"status" -> "ok",
					"data" -> list.toList
				)
			case StringExpression( s ) =>
				def replacer( m: Regex.Match ): String = {
					vars get m.group(1) match {
						case None => sys.error( "variable not found: " + m.group(1) )
						case Some( s ) => s
					}
				}
	
				varRegex.replaceAllIn( s, replacer _ )
			case VariableExpression( v ) =>
				v match {
					case "json" =>
						DefaultJSONReader.fromString( reqbody )
					case _ if vars contains v => vars(v)
					case _ if tables contains v => tables(v)
					case _ => sys.error( "variable not found: " + v )
				}
			case _ => sys.error( "error evaluating expression" )
		}
	
	def evals( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String ): String =
		eval( expr, vars, tables, reqbody ).asInstanceOf[String]
	
	def evali( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String ): Int =
		eval( expr, vars, tables, reqbody ).asInstanceOf[String].toInt
	
	def evalj( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String ): Map[String, Any] =
		eval( expr, vars, tables, reqbody ).asInstanceOf[Map[String, Any]]
	
	def find( method: String, path: String, routes: List[Route] ): Option[(Map[String,String], ExpressionAST)] = {
		if (routes eq null)
			sys.error( "no routes loaded" )
			
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
			
			if (len == uri.length && method.toUpperCase == rmethod && uri.zip( segments ).forall {
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
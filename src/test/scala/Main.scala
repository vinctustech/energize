package xyz.hyperreal.informatio

import java.sql._

import collection.mutable.{HashMap, ListBuffer}
import util.parsing.input.CharSequenceReader

import xyz.hyperreal.indentation_lexical._


object Main extends App {
	
	val p = new InformatioParser
	val ast =
		p.parse( new CharSequenceReader(io.Source.fromFile("t0.info").getLines.map(l => l + '\n').mkString) ) match {
			case p.Success( tree, _ ) => tree
			case p.NoSuccess( error, rest ) =>
				println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
				sys.exit
				sys.error( "" )
		}

//		println( ast )
	Class.forName( "org.h2.Driver" )
	
	val conn = DriverManager.getConnection( "jdbc:h2:~/projects/informatio/test", "sa", "" )
	val statement = conn.createStatement
	
	def problem( error: String ) {
		conn.close
		sys.error( error )
	}
	
	val routes = new ListBuffer[Route]
	
	ast foreach {
		case TableDefinition( name, fields ) =>
			if (!conn.getMetaData.getTables( null, null, name.toUpperCase, null ).next) {
				val f =
					fields map {
						case TableField( modifiers, typ, name ) =>
							val t =
								typ match {
									case StringType => "VARCHAR(255)"
									case UUIDType => "UUID"
									case DateType => "DATE"
								}
								
							name + " " + t
					} mkString ", "
				
				val com = "CREATE TABLE " + name + "(" + "id INT AUTO_INCREMENT PRIMARY KEY, " + f + ")"
				
				println( com )
				statement.execute( com )
			}
		case RoutesDefinition( URIPath(base), mappings ) =>
//			println( base map {case NameURISegment(segment) => segment} mkString ("/", "/", "/") )
			
			mappings foreach {
				case URIMapping( GETMethod, URIPath(path), action ) => routes += Route( "GET", base ++ path, action )
				case URIMapping( POSTMethod, URIPath(path), action ) => routes += Route( "POST", base ++ path, action )
				case URIMapping( PUTMethod, URIPath(path), action ) => routes += Route( "PUT", base ++ path, action )
				case URIMapping( DELETEMethod, URIPath(path), action ) => routes += Route( "DELETE", base ++ path, action )
				case _ => sys.error( "unknown method" )
			}
	}
	
//	println( routes mkString "\n" )
	
	println( find("GET", "/api/v1/users/joe") )
	println( find("GET", "/api/v1/users") )
	
	conn.close
	
	def find( method: String, path: String ): Option[(Map[String,String], ExpressionAST)] = {
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
	
	case class Route( method: String, path: List[URISegment], action: ExpressionAST )
}
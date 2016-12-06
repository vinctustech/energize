package xyz.hyperreal

import java.sql._

import collection.mutable.{ListBuffer, HashMap}
import util.matching.Regex
import util.parsing.input.{Position}
import collection.mutable.{LinkedHashMap, HashMap, ListBuffer}

import xyz.hyperreal.table.TextTable
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}
import xyz.hyperreal.lia.Math


package object cras {
	
	private val varRegex = """\$([a-zA-Z][a-zA-Z0-9]*)""".r
	
	def dbconnect( dbfile: String, memory: Boolean = false ) = {
		Class.forName( "org.h2.Driver" )
		
		val connection = DriverManager.getConnection( s"jdbc:h2${if (memory) ":mem:" else ":"}" + dbfile, "sa", "" )
		
		(connection, connection.createStatement)
	}

	val URI = """(/(?:[a-zA-Z0-9_-]/)*)(?:\?((?:[a-zA-Z]=.*&?)+))?"""r
	
	def process( reqmethod: String, requri: String, reqbody: String, env: Env ) = {
		val reqpath = requri
		
		find( reqmethod, reqpath, env.routes ) match {
			case None =>
				None
			case Some( (urivars, expr) ) =>
				try {
					val reqvars =
						if (reqbody eq null)
							urivars
						else
							urivars + ("json" -> DefaultJSONReader.fromString(reqbody))
							
					Some( DefaultJSONWriter.toString(evalm( expr, env add reqvars )) )
				} catch {
					case e: CrasErrorException => Some( DefaultJSONWriter.toString(Map("status" -> "error", "reason" -> e.getMessage)) )
					case e: CrasNotFoundException => None
				}
		}
	}

	def eval( expr: String, env: Env ): Any = {
		val p = new CrasParser
		val ast = p.parseFromString( expr, p.expressionStatement )
		
		eval( ast.expr, env )	
	}
	
	def replacer( env: Env ) = (m: Regex.Match) => env lookup m.group(1) toString
	
	def eval( expr: ExpressionAST, env: Env ): Any =
		expr match {
			case DotExpression( obj, field ) => evalm( obj, env )( field )
			case BinaryExpression( left, op, func, right ) =>
				val l = eval( left, env )
				val r = eval( right, env )
				
				if (op == '+) {
					if (l.isInstanceOf[String] || r.isInstanceOf[String])
						String.valueOf( l ) + String.valueOf( r )
					else if (l.isInstanceOf[Map[String, Any]] && r.isInstanceOf[Map[String, Any]])
						l.asInstanceOf[Map[String, Any]] ++ r.asInstanceOf[Map[String, Any]]
					else
						Math( func, l, r )
				}
				else if (op == '* && l.isInstanceOf[String] && r.isInstanceOf[Int])
					l.asInstanceOf[String]*r.asInstanceOf[Int]
				else
					Math( func, l, r )
			case ApplyExpression( function, args ) =>
				eval( function, env ) match {
					case f: Native =>
						val list = args map (a => eval( a, env ))
						
						if (list.length != f.argc)
							sys.error( "wrong number of arguments for native function: " + f )
							
						f( list, env )
					case f: FunctionExpression =>
						if (f.params.length != args.length)
							sys.error( "wrong number of arguments for function: " + f )
							
						eval( f.expr, env add (f.params zip (args map (a => eval( a, env )))).toMap )
				}
			case LiteralExpression( s: String ) => varRegex.replaceAllIn( s, replacer(env) )
			case LiteralExpression( v ) => v
			case VariableExpression( n ) => env lookup n
			case ObjectExpression( pairs ) =>
				Map( pairs map {case (k, v) => (k, eval(v, env))}: _* )
					case _ => sys.error( "error evaluating expression: " + expr )
		}
	
	def evals( expr: ExpressionAST, env: Env ) =
		eval( expr, env ).asInstanceOf[String]
	
	def evali( expr: ExpressionAST, env: Env ) =
		eval( expr, env ).asInstanceOf[String].toInt
	
	def evalm( expr: ExpressionAST, env: Env )=
		eval( expr, env ).asInstanceOf[Map[String, Any]]
	
	def evalb( expr: ExpressionAST, env: Env ) =
		eval( expr, env ).asInstanceOf[Boolean]
	
	def find( method: String, path: String, routes: List[Route] ): Option[(Map[String, Any], ExpressionAST)] = {
		if (routes eq null)
			sys.error( "no routes loaded" )
			
		val segments = {
			val trimed = path.trim
			val l = trimed split "/" toList
			
			if (l == List( "" ))
				return None
			else if (!l.isEmpty && l.head != "")
				return None
			else
				if (l == List())
					l
				else
					l.tail
			}
		val len = segments.length
		val urivars = new HashMap[String, String]
		
		for (Route( rmethod, uri, action) <- routes) {
			urivars.clear
			
			if (len == uri.length && method.toUpperCase == rmethod && uri.zip( segments ).forall {
				case (NameURISegment( route ), request) => route == request
				case (ParameterURISegment( name ), request) =>
					urivars(name) = request
					true
			})
				return Some( (urivars.toMap, action) )
		}
		
		None
	}
	
	def problem( pos: Position, error: String ) = sys.error( pos.line + ": " + error + "\n" + pos.longString )
	
	def configure( src: io.Source, connection: Connection, statement: Statement ): Env = {
		val p = new CrasParser
		val ast = p.parseFromSource( src, p.source )
		val tables = new HashMap[String, Table]
		val routes = new ListBuffer[Route]
		val defines = new HashMap[String, Any]
		
		interpret( ast )
		
		def env = Env( tables.toMap, routes.toList, Builtins.map ++ defines, connection, statement )
		
		def traverse( list: List[AST] ) = list foreach interpret
		
		def interpret( ast: AST ): Unit =
			ast match {
				case SourceAST( list ) => traverse( list )
				case d@VariableDefinition( name, expr ) =>
					if (defines contains name)
						problem( d.pos, s"'$name' already defined" )
						
					defines(name) = eval( expr, env )
				case FunctionDefinition( pos, name, function ) =>
					if (defines contains name)
						problem( pos, s"'$name' already defined" )
						
					defines(name) = function
				case TableDefinition( pos, name, bases, columns ) =>
					if (tables contains name.toUpperCase)
						problem( pos, s"'$name' already defined" )
					
					val cols = new LinkedHashMap[String, Column]
					val create = new StringBuilder
					
					create ++= "CREATE TABLE "
					create ++= name
					create ++= "(id INT NOT NULL AUTO_INCREMENT PRIMARY KEY, "
					create ++=
						columns map {
							case TableColumn( pos, modifiers, typ, cname ) =>
								if (cols contains cname.toUpperCase)
									problem( pos, s"column '$cname' defined twice" )
									
								val t =
									typ match {
										case StringType => "VARCHAR(255)"
										case IntegerType => "INT"
										case UUIDType => "UUID"
										case DateType => "DATE"
										case TableType( _ ) => "INT"
									}
								var secret = false
								var required = false
								var optional = false
								var unique = false
								var m = ""
								
								modifiers foreach {
									case ColumnTypeModifier( "unique", pos ) =>
										if (unique)
											problem( pos, "modifier 'unique' encountered more than once" )
											
										unique = true
										m += " unique"
									case ColumnTypeModifier( "required", pos ) =>
										if (required)
											problem( pos, "modifier 'required' encountered more than once" )
											
										if (optional)
											problem( pos, "modifier 'required' encountered along with 'optional'" )
											
										m += " not null"
										required = true
									case ColumnTypeModifier( "optional", pos ) =>
										if (optional)
											problem( pos, "modifier 'optional' encountered more than once" )
											
										if (required)
											problem( pos, "modifier 'optional' encountered along with 'required'" )
											
										optional = true
									case ColumnTypeModifier( "secret", pos ) =>
										if (secret)
											problem( pos, "modifier 'secret' encountered more than once" )
											
										secret = true	
								}
								
								cols(cname.toUpperCase) = Column( cname, typ, secret, required )
								cname + " " + t + m
						} mkString ", "
					create ++= ");"
			
					if (!connection.getMetaData.getTables( null, "PUBLIC", name.toUpperCase, null ).next) {
						statement.execute( create.toString )
					}
		
					tables(name.toUpperCase) = Table( name, cols map {case (_, cinfo) => cinfo.name} toList, cols.toMap )
					
					if (bases isEmpty) {
						val Env( _, r, _, _, _ ) = configure( io.Source.fromString(Builtins.routes.replaceAll("<base>", "").replaceAll("<table>", name)), null, null )
							
						routes ++= r
					} else {
						for (URIPath( base ) <- bases) {
							val Env( _, r, _, _, _ ) = configure( io.Source.fromString(Builtins.routes.replaceAll("<table>", name).
								replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString ("/", "/", ""))), null, null )
							
							routes ++= r
						}
					}
				case RoutesDefinition( URIPath(base), mappings ) =>
					mappings foreach {
						case URIMapping( HTTPMethod(method), URIPath(path), action ) =>
							routes += Route( method, base ++ path, action )
					}
				case ExpressionStatement( expr ) => eval( expr, env )
			}
		
		if (!defines.contains( "OK" ))
			defines("OK") = OKNative
			
		if (!defines.contains( "Error" ))
			defines("Error") = ErrorNative
		
		env
	}
	
	class CrasErrorException( message: String ) extends Exception( message )
	
	class CrasNotFoundException extends Exception
	
}
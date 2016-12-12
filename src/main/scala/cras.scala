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
	
//	type JSON = Map[String, Any]
	
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
					val res = evalm( expr, env add reqvars )
					
					Some( if (res eq null) null else DefaultJSONWriter.toString(res) )
				} catch {
					case e: CrasErrorException =>
						Some( DefaultJSONWriter.toString(env.variables("errorResult").asInstanceOf[Native](List(e.getMessage), env).asInstanceOf[Map[String, Any]]) )
					case e: CrasNotFoundException => None
				}
		}
	}

	def evaluate( expr: String, env: Env ): Any = {
		val p = new CrasParser
		val ast = p.parseFromString( expr, p.expressionStatement )
		
		deref( ast.expr, env )	
	}
	
	def replacer( env: Env ) = (m: Regex.Match) => env lookup m.group(1) toString
	
	def eval( expr: ExpressionAST, env: Env ): Any =
		expr match {
			case DotExpression( obj, field ) => evalm( obj, env )( field )
			case CompoundExpression( left, right ) =>
				deref( left, env )
				deref( right, env )
			case BinaryExpression( left, op, func, right ) =>
				val l = deref( left, env )
				val r = deref( right, env )
				
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
				deref( function, env ) match {
					case f: Native =>
						val list = args map (a => deref( a, env ))
						
						if (f.applicable( list ))
							f( list, env )
						else
							sys.error( "wrong number or type of arguments for native function: " + f )
					case f: FunctionExpression =>
						if (f.params.length != args.length)
							sys.error( "wrong number of arguments for function: " + f )
							
						deref( f.expr, env add (f.params zip (args map (a => deref( a, env )))).toMap )
				}
			case LiteralExpression( s: String ) => varRegex.replaceAllIn( s, replacer(env) )
			case LiteralExpression( v ) => v
			case VariableExpression( n ) => env lookup n
			case ObjectExpression( pairs ) =>
				Map( pairs map {case (k, v) => (k, deref(v, env))}: _* )
					case _ => sys.error( "error evaluating expression: " + expr )
		}
	
	def deref( expr: ExpressionAST, env: Env ) =
		eval( expr, env ) match {
			case h: Variable => h.value
			case v => v
		}

	def evalv( expr: ExpressionAST, env: Env ) =
		eval( expr, env ).asInstanceOf[Variable]

	def evals( expr: ExpressionAST, env: Env ) =
		deref( expr, env ).asInstanceOf[String]
	
	def evali( expr: ExpressionAST, env: Env ) =
		deref( expr, env ).asInstanceOf[String].toInt
	
	def evalm( expr: ExpressionAST, env: Env )=
		deref( expr, env ).asInstanceOf[Map[String, Any]]
	
	def evalb( expr: ExpressionAST, env: Env ) =
		deref( expr, env ).asInstanceOf[Boolean]
	
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
		val urivars = new HashMap[String, Any]
		
		def integer( s: String ) = {
			if (s forall (c => c.isDigit))
				Some( BigInt(s) )
			else
				None
		}
		
		for (Route( rmethod, uri, action) <- routes) {
			urivars.clear
			
			if (len == uri.length && method.toUpperCase == rmethod && uri.zip( segments ).forall {
				case (NameURISegment( route ), segment) => route == segment
				case (ParameterURISegment( name, "string" ), segment) =>
					urivars(name) = segment
					true
				case (ParameterURISegment( name, "integer" ), segment) =>
					integer( segment ) match {
						case Some( a ) if a.isValidInt =>
							urivars(name) = a.intValue
							true
						case _ => false
					}
				case (ParameterURISegment( name, "long" ), segment) =>
					integer( segment ) match {
						case Some( a ) if a.isValidLong =>
							urivars(name) = a.longValue
							true
						case _ => false
					}
				case _ => false
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
		val create = new StringBuilder
		
		def env = Env( tables.toMap, routes.toList, Builtins.map ++ defines, connection, statement )
		
		def traverseDefinitions( list: List[AST] ) = list foreach interpretDefinitions
		
		def interpretDefinitions( ast: AST ): Unit =
			ast match {
				case SourceAST( list ) => traverseDefinitions( list )
				case d@VariableDefinition( name, expr ) =>
					if (defines contains name)
						problem( d.pos, s"'$name' already defined" )
						
					defines(name) = new Variable( deref(expr, env) )
				case d@ValueDefinition( name, expr ) =>
					if (defines contains name)
						problem( d.pos, s"'$name' already defined" )
						
					defines(name) = deref( expr, env )
				case FunctionDefinition( pos, name, function ) =>
					if (defines contains name)
						problem( pos, s"'$name' already defined" )
						
					defines(name) = function
				case TableDefinition( pos, name, bases, columns ) =>
					if (tables contains name.toUpperCase)
						problem( pos, s"'$name' already defined" )
					
					val cols = new LinkedHashMap[String, Column]
					
					create ++= "CREATE TABLE "
					create ++= name
					create ++= "(id INT NOT NULL AUTO_INCREMENT PRIMARY KEY, "
					create ++=
						columns map {
							case col@TableColumn( modifiers, typ, cname ) =>
								if (cols contains cname.toUpperCase)
									problem( col.pos, s"column '$cname' defined twice" )
									
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
									case tm@ColumnTypeModifier( "unique" ) =>
										if (unique)
											problem( tm.pos, "modifier 'unique' encountered more than once" )
											
										unique = true
										m += " UNIQUE"
									case tm@ColumnTypeModifier( "required" ) =>
										if (required)
											problem( tm.pos, "modifier 'required' encountered more than once" )
											
										if (optional)
											problem( tm.pos, "modifier 'required' encountered along with 'optional'" )
											
										m += " NOT NULL"
										required = true
									case tm@ColumnTypeModifier( "optional" ) =>
										if (optional)
											problem( tm.pos, "modifier 'optional' encountered more than once" )
											
										if (required)
											problem( tm.pos, "modifier 'optional' encountered along with 'required'" )
											
										optional = true
									case tm@ColumnTypeModifier( "secret" ) =>
										if (secret)
											problem( tm.pos, "modifier 'secret' encountered more than once" )
											
										secret = true
								}
								
								cols(cname.toUpperCase) = Column( cname, typ, secret, required )
								cname + " " + t + m
						} mkString ", "
					create ++= ");\n"
		
					tables(name.toUpperCase) = Table( name, cols map {case (_, cinfo) => cinfo.name} toList, cols.toMap )
					
					if (bases isEmpty) {
						val Env( _, r, _, _, _ ) = configure( io.Source.fromString(Builtins.routes.replaceAll("<base>", "").replaceAll("<resource>", name)), null, null )
							
						routes ++= r
					} else {
						for (URIPath( base ) <- bases) {
							val Env( _, r, _, _, _ ) = configure( io.Source.fromString(Builtins.routes.replaceAll("<resource>", name).
								replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString ("/", "/", ""))), null, null )
							
							routes ++= r
						}
					}
				case RoutesDefinition( URIPath(base), mappings ) =>
					mappings foreach {
						case URIMapping( HTTPMethod(method), URIPath(path), action ) =>
							routes += Route( method, base ++ path, action )
					}
				case _ =>
			}
		
		def traverseExpressions( list: List[AST] ) = list foreach interpretExpressions
		
		def interpretExpressions( ast: AST ): Unit =
			ast match {
				case SourceAST( list ) => traverseExpressions( list )
				case ExpressionStatement( expr ) => deref( expr, env )
				case _ =>
			}
			
		interpretDefinitions( ast )	
			
		if (!tables.isEmpty && !connection.getMetaData.getTables( null, "PUBLIC", tables.head._1, null ).next) {
			statement.execute( create.toString )
		}
		
		interpretExpressions( ast )	
		env
	}
	
	class CrasErrorException( message: String ) extends Exception( message )
	
	class CrasNotFoundException extends Exception
	
	class Variable( var value: Any )
}
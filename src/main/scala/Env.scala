package xyz.hyperreal.cras

import java.sql._
import java.net.URI

import collection.JavaConverters._
import util.matching.Regex
import collection.mutable.{HashMap, MultiMap, Set}

import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.NameValuePair

import xyz.hyperreal.lia.Math
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


case class Env( tables: Map[String, Table], routes: List[Route], variables: Map[String, Any], connection: Connection, statement: Statement ) {
	
	private val varRegex = """\$([a-zA-Z][a-zA-Z0-9]*)""".r

//	private val URI = """(/(?:[a-zA-Z0-9_-]/)*)(?:\?((?:[a-zA-Z]=.*&?)+))?"""r
	
	def add( kv: (String, Any) ) = Env( tables, routes, variables + kv, connection, statement )
	
	def add( m: Map[String, Any] ) = Env( tables, routes, variables ++ m, connection, statement )
	
	def get( name: String ) =
		variables get name match {
			case None => tables get name.toUpperCase
			case res => res
		}
	
	def lookup( name: String ) =
		get( name ) match {
			case None => throw new CrasErrorException( "variable not found: " + name )
			case Some( v ) => v
		}
	
	def process( reqmethod: String, requri: String, reqbody: String ) = {
		val uri = new URI( requri )
		val reqpath = uri.getPath
		val reqquery = URLEncodedUtils.parse( uri, "UTF-8" ).asScala map (p => (p.getName, p.getValue)) toMap
		
//			val map1 = new HashMap[String, Set[String]] with MultiMap[String, String]
		
//			URLEncodedUtils.parse( uri, "UTF-8" ).asScala map (p => map1.addBinding( p.getName, p.getValue ))

		// 	var map = Map[String, Any]()
			
		// 	URLEncodedUtils.parse( uri, "UTF-8" ).asScala foreach (
		// 		p =>
		// 			map get p.getName match {
		// 				case None => map += (p.getName -> p.getValue)
		// 				case Some( l: List[_] ) => map += (p.getName -> (l :+ p.getValue))
		// 				case Some( v ) => map += (p.getName -> List( v, p.getValue ))
		// 			})
		// 	map
		// }
		println( reqquery )
		
		val reqfrag = uri.getFragment
		
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
		
		find( reqmethod, reqpath, routes ) match {
			case None => None
			case Some( (urivars, expr) ) =>
				try {
					val reqvars =
						(if (reqbody eq null)
							urivars
						else
							urivars + ("json" -> DefaultJSONReader.fromString(reqbody))) ++ reqquery
					val res = (this add reqvars).evalm( expr )
					
					Some( if (res eq null) null else DefaultJSONWriter.toString(res) )
				} catch {
					case e: CrasErrorException =>
						Some( DefaultJSONWriter.toString(variables("errorResult").asInstanceOf[Native](List(e.getMessage), this).asInstanceOf[Map[String, Any]]) )
					case e: CrasNotFoundException => None
				}
		}
	}

	def evaluate( expr: String ): Any = {
		val p = new CrasParser
		val ast = p.parseFromString( expr, p.expressionStatement )
		
		deref( ast.expr )	
	}
	
	def replacer = (m: Regex.Match) => lookup( m.group(1) ).toString
	
	def eval( expr: ExpressionAST ): Any =
		expr match {
			case DotExpression( obj, field ) => evalm( obj )( field )
			case CompoundExpression( left, right ) =>
				deref( left )
				deref( right )
			case BinaryExpression( left, op, func, right ) =>
				val l = deref( left )
				val r = deref( right )
				
				if (op == '+) {
					if (l.isInstanceOf[String] || r.isInstanceOf[String])
						String.valueOf( l ) + String.valueOf( r )
					else if (l.isInstanceOf[Map[_, _]] && r.isInstanceOf[Map[_, _]])
						l.asInstanceOf[Map[String, Any]] ++ r.asInstanceOf[Map[String, Any]]
					else
						Math( func, l, r )
				}
				else if (op == '* && l.isInstanceOf[String] && r.isInstanceOf[Int])
					l.asInstanceOf[String]*r.asInstanceOf[Int]
				else
					Math( func, l, r )
			case ApplyExpression( function, args ) =>
				deref( function ) match {
					case f: Native =>
						val list = args map (a => deref( a ))
						
						if (f.applicable( list ))
							f( list, this )
						else
							sys.error( "wrong number or type of arguments for native function: " + f )
					case f: FunctionExpression =>
						if (f.params.length != args.length)
							sys.error( "wrong number of arguments for function: " + f )
							
						(this add (f.params zip (args map (a => deref( a )))).toMap).deref( f.expr )
				}
			case LiteralExpression( s: String ) => varRegex.replaceAllIn( s, replacer )
			case LiteralExpression( v ) => v
			case VariableExpression( n ) => lookup( n )
			case OptVariableExpression( n ) => get( n )
			case ObjectExpression( pairs ) =>
				Map( pairs map {case (k, v) => (k, deref(v))}: _* )
			case ConditionalExpression( cond, no ) =>
				def condition( ifthen: List[(ExpressionAST, ExpressionAST)] ): Any =
					ifthen match {
						case Nil =>
							no match {
								case None => null
								case Some( expr ) => eval( expr )
							}
						case (ifexpr, thenexpr) :: tail =>
							if (evalb( ifexpr ))
								eval( thenexpr )
							else
								condition( tail )
					}
					
				condition( cond )
			case ComparisonExpression( left, comps ) =>
				var l = eval( left )
				
				comps forall {
					case (_, func, right) =>
						val r = eval( right )
						
						if (Math( func, l, r ).asInstanceOf[Boolean]) {
							l = r
							true
						} else
							false
				}
			case BlockExpression( exprs ) =>
				def block( list: List[StatementAST] ): Any =
					list match {
						case ExpressionStatement( h ) :: Nil => eval( h )
						case ExpressionStatement( h ) :: t =>
							eval( h )
							block( t )
						case _ => sys.error( "something bad has happened" )
					}
					
				block( exprs )
			case _ => sys.error( "error evaluating expression: " + expr )
		}
	
	def deref( expr: ExpressionAST ) =
		eval( expr ) match {
			case h: Variable => h.value
			case v => v
		}

	def evalv( expr: ExpressionAST ) = eval( expr ).asInstanceOf[Variable]

	def evals( expr: ExpressionAST ) = deref( expr ).asInstanceOf[String]
	
	def evali( expr: ExpressionAST ) = deref( expr ).asInstanceOf[String].toInt
	
	def evalm( expr: ExpressionAST ) = deref( expr ).asInstanceOf[Map[String, Any]]
	
	def evalb( expr: ExpressionAST ) = deref( expr ).asInstanceOf[Boolean]

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )

case class Table( name: String, names: List[String], columns: Map[String, Column] )

case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean )
	
class CrasErrorException( message: String ) extends Exception( message )

class CrasNotFoundException extends Exception

class Variable( var value: Any )

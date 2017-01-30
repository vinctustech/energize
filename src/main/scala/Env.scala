package xyz.hyperreal.energize

import java.sql._
import java.net.URI

import collection.JavaConverters._
import util.matching.Regex
import collection.mutable.HashMap

import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.HttpStatus._

import xyz.hyperreal.lia.Math
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


case class Env( tables: Map[String, Table], routes: List[Route], variables: Map[String, Any], connection: Connection, statement: Statement, db: Database ) {

	private val varRegex = """\$\$|\$([a-zA-Z][a-zA-Z0-9]*)""".r

//	private val URI = """(/(?:[a-zA-Z0-9_-]/)*)(?:\?((?:[a-zA-Z]=.*&?)+))?"""r
	
	def add( kv: (String, Any) ) = Env( tables, routes, variables + kv, connection, statement, db )
	
	def add( m: Map[String, Any] ) = Env( tables, routes, variables ++ m, connection, statement, db )
	
	def get( name: String ) =
		variables get name match {
			case None => tables get db.desensitize( name )
			case res => res
		}
	
	def lookup( name: String ) = get( name ) getOrElse (sys.error( "variable not found: " + name ))

	def process( reqmethod: String, requri: String, reqbody: String ): (Int, String) = {
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
		
//		val reqfrag = uri.getFragment
		
		def find( method: String, path: String, routes: List[Route] ): Option[(Map[String, Any], ExpressionAST)] = {
			if (routes eq null)
				sys.error( "no routes loaded" )
				
			val segments = {
				val trimed = path.trim
				val l = trimed split "/" toList
				
				if (l == List( "" ))
					return None
				else if (l.nonEmpty && l.head != "")
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
			case None =>
				val (sc, obj) = ResultFunctions.NotFound( this, "route not found" )

				(sc, DefaultJSONWriter.toString( obj ))
			case Some( (urivars, expr) ) =>
				val reqvars =
					(if (reqbody eq null)
						urivars
					else
						urivars + ("json" -> DefaultJSONReader.fromString(reqbody))) ++ reqquery
				val (sc, obj) =
					try {
						(this add reqvars).deref( expr ).asInstanceOf[(Int, OBJ)]
					} catch {
						case e: BadRequestException =>
							ResultFunctions.BadRequest( this, e.getMessage )
						case e: NotFoundException =>
							ResultFunctions.NotFound( this, e.getMessage )
						case e: org.h2.jdbc.JdbcSQLException if e.getMessage startsWith "Unique index or primary key violation" =>
							ResultFunctions.Conflict( this, e.getMessage )
					}

				(sc, if (obj eq null) null else DefaultJSONWriter.toString( obj ))
		}
	}

	def evaluate( expr: String ): Any = {
		val p = new EnergizeParser
		val ast = p.parseFromString( expr, p.expressionStatement )
		
		deref( ast.expr )	
	}

	def replacer =
		(m: Regex.Match) =>
			Regex quoteReplacement(
				if (m.group( 1 ) eq null)
					"$"
				else
					lookup( m group 1 ).toString
				)

	def eval( expr: ExpressionAST ): Any =
		expr match {
			case AssignmentExpression( v, expr ) =>
				variables get v match {
					case None => sys.error( s"variable '$v' not found" )
					case Some( h: Variable ) => h.value = deref( expr )
					case _ => sys.error( s"'$v' is not a variable" )
				}
			case RangeExpression( start, end ) => evalbi( start ) to evalbi( end )
			case ListExpression( exprs ) => exprs map deref
			case TupleExpression( first, rest ) =>
				val e1 = deref( first )
				val r = rest map deref

				r match {
					case List( e2 ) => (e1, e2)
					case List( e2, e3 ) => (e1, e2, e3)
					case List( e2, e3, e4 ) => (e1, e2, e3, e4)
					case List( e2, e3, e4, e5 ) => (e1, e2, e3, e4, e5)
				}
			case DotExpression( obj, field ) => evalm( obj )( field )
			case CompoundExpression( left, right ) =>
				eval( left )
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
			case ApplyExpression( function, pos, args ) =>
				deref( function ) match {
					case f: Native =>
						val list = args map (a => deref( a ))
						
						if (f.applicable( list ))
							f( list, this )
						else
							problem( pos, "wrong number or type of arguments for native function: " + f )
					case f: FunctionExpression =>
						if (f.params.length != args.length)
							problem( pos, "wrong number of arguments for function: " + f )
							
						(this add (f.params zip (args map (a => deref( a )))).toMap).deref( f.expr )
					case m: Map[_, _] =>
						if (args.tail != Nil)
							problem( pos, "can only apply a map to one argument" )

						m.asInstanceOf[Map[String, Any]]( evals(args.head) )
				}
			case LiteralExpression( s: String ) => varRegex.replaceAllIn( s, replacer )
			case LiteralExpression( v ) => v
			case VariableExpression( n ) => lookup( n )
			case OptVariableExpression( n ) => variables get n
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
			case ForExpression( gen, body, e ) =>
				def forloop( env: Env, gs: List[GeneratorAST] ) {
					gs match {
						case List( g ) =>
							val GeneratorAST( pattern, traversable, filter ) = g
							val tr = evalt( traversable )
							
							tr foreach (item => env.add( pattern -> item ).eval( body ))
						case hd :: tl =>
							val GeneratorAST( pattern, traversable, filter ) = hd
							val tr = evalt( traversable )
							
							tr foreach (item => forloop( env.add(pattern -> item), tl ))
					}
				}

				forloop( this, gen )
			case WhileExpression( cond, body, e ) =>
				while (evalb( cond ))
					eval( body )
			case ComparisonExpression( left, comps ) =>
				var l = deref( left )
				
				comps forall {
					case (_, func, right) =>
						val r = deref( right )
						
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
	
	def evalbi( expr: ExpressionAST ) =
		deref( expr ) match {
			case a: Int => BigInt( a )
			case a: BigInt => a
		}
	
	def evalm( expr: ExpressionAST ) = deref( expr ).asInstanceOf[Map[String, Any]]

	def evalp( expr: ExpressionAST ) = deref( expr ).asInstanceOf[(AnyRef, AnyRef)]

	def evalb( expr: ExpressionAST ) = deref( expr ).asInstanceOf[Boolean]
	
	def evalt( expr: ExpressionAST ) = deref( expr ).asInstanceOf[TraversableOnce[Any]]

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )

case class Table(name: String, columns: List[Column], columnMap: Map[String, Column], resource: Boolean, mtm: Boolean, var preparedInsert: PreparedStatement ) {
	def names = columns map (c => c.name)
}

case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean )

class Variable( var value: Any )

class NotFoundException( error: String ) extends Exception( error )

class BadRequestException( error: String ) extends Exception( error )

class ForbiddenException( error: String ) extends Exception( error )
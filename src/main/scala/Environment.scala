package xyz.hyperreal.energize

import java.sql.{Connection, Statement, SQLException, PreparedStatement}
import java.net.URI
import javax.script._
import jdk.nashorn.api.scripting._

import collection.JavaConverters._
import util.matching.Regex
import collection.mutable.{ArrayBuffer, HashMap}

import org.apache.http.client.utils.URLEncodedUtils

import xyz.hyperreal.lia.Math
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


object Environment {
	val varRegex = """\$\$|\$([a-zA-Z][a-zA-Z0-9]*)""".r
}

// todo: add RouteTable class to speed up adding variables
class Environment( val tables: Map[String, Table], croutes: List[Route], val bindings: Map[String, Any],
									 val sbindings: Map[String, SystemValue], val connection: Connection, val statement: Statement,
									 val db: Database, var pathMap: Map[String, Any], var queryMap: Map[String, String],
									 val key: String ) {

	private val routeTable = ArrayBuffer( croutes: _* )
	private var routeList = routeTable.toList
	private lazy val entityVariable = sbindings( "entity" ).asInstanceOf[SystemVariable]

	val jseng = new ScriptEngineManager( null ).getEngineByName( "nashorn" ).asInstanceOf[NashornScriptEngine]

	for ((k, v) <- Builtins.jsmap)
		v match {
			case n: Native => jseng.put( k, new JSWrapper(this, n) )
			case _ => jseng.put( k, v )
		}

//	private val URI = """(/(?:[a-zA-Z0-9_-]/)*)(?:\?((?:[a-zA-Z]=.*&?)+))?"""r

	def routes = routeList

	def discard: Unit = {
		connection.close
	}

	def add( kv: (String, Any) ) = new Environment( tables, routes, bindings + kv, sbindings, connection, statement, db, pathMap, queryMap, key )

	def add( m: collection.Map[String, Any] ) = new Environment( tables, routes, bindings ++ m, sbindings, connection, statement, db, pathMap, queryMap, key )

	def remove( n: String ) = new Environment( tables, routes, bindings - n, sbindings, connection, statement, db, pathMap, queryMap, key )

	def get( name: String ) =
		bindings get name match {
			case None => tables get db.desensitize( name )
			case res => res
		}

	def table( name: String ) = tables(db.desensitize(name)).asInstanceOf[Table]

	def lookup( name: String ) = get( name ) getOrElse sys.error( "variable not found: " + name )

	def native( name: String, args: Any* ) = bindings( name ).asInstanceOf[Native]( this, args.toList )

	def result( name: String, args: Any* ) = native( name, args: _* ).asInstanceOf[(Int, String, OBJ)]

	def add( route: Route ) = routeTable += route

	def remove( method: String, path: URIPath ): Unit = {
		for (i <- 0 until routeTable.length)
			if (routeTable(i).method == method && routeTable(i).path == path) {
				routeTable.remove( i )
				return
			}

		sys.error( s"route not found: $method $path" )
	}

	def process( reqmethod: String, requri: String, reqbody: String ): (Int, String, AnyRef) = {
		def notfound = {
			val (sc, ctype, obj) = result( "NotFound", "route not found" )

			(sc, ctype, DefaultJSONWriter.toString( obj ))
		}

		val uri = new URI( requri )
		val reqpath = uri.getPath
		val reqquery = Map( URLEncodedUtils.parse( uri, "UTF-8" ).asScala map (p => (p.getName, p.getValue)): _* )

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

			if (routes eq null)
				sys.error( "no routes loaded" )

			val segments = {
				val trimed = reqpath.trim
				val l = trimed split "/" toList

				if (l == List( "" ))
					return notfound
				else if (l.nonEmpty && l.head != "")
					return notfound
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

			for (Route( rmethod, URIPath(rsegments), action) <- routes) {
				urivars.clear

				if (len == rsegments.length && reqmethod.toUpperCase == rmethod && rsegments.zip( segments ).forall {
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
				}) {
					try {
						val (sc, ctype, obj) =
							try {
								entityVariable.value = if (reqbody eq null) null else DefaultJSONReader.fromString( reqbody )
								pathMap = urivars.toMap
								queryMap = reqquery
								deref( action ).asInstanceOf[(Int, String, AnyRef)]
							} catch {
								case e: UnauthorizedException =>
									result( "Unauthorized", s"""realm="${e.getMessage}"""" )
								case e: ExpiredException =>
									result( "Unauthorized", s"""realm="${e.getMessage}", error="invalid_token", error_description="The access token expired"""" )
								case e: ForbiddenException =>
									result( "Forbidden", e.getMessage )
								case e: BadRequestException =>
									result( "BadRequest", e.getMessage )
								case e: NotFoundException =>
									result( "NotFound", e.getMessage )
								case e: SQLException if db.conflict( e.getMessage )  =>
									result( "Conflict", e.getMessage )
							}

						return (sc, ctype, obj match {
//								case null => null
//								case s: String => s
								case o: Map[_, _] => DefaultJSONWriter.toString( o.asInstanceOf[OBJ] )
								case _ => obj
							})
					} catch {
						case _: RejectRouteThrowable =>
					}
				}
			}

			notfound
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
			case AssignmentExpression( v, e ) =>
				bindings get v match {
					case None => sys.error( s"variable '$v' not found" )
					case Some( h: Variable ) => h.value = deref( e )
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
						val list = args map deref

						if (f.applicable( list ))
							f( this, list )
						else
							problem( pos, "wrong number or type of arguments for native function: " + f )
					case f: FunctionExpression =>
						if (f.params.length != args.length)
							problem( pos, "wrong number of arguments for function: " + f )
							
						(this add (f.params zip (args map (a => deref( a )))).toMap).deref( f.expr )
					case m: Map[_, _] =>
						if (args == Nil || args.tail != Nil)
							problem( pos, "can only apply a map to one argument" )

						m.asInstanceOf[Map[String, Any]]( evals(args.head) )
				}
			case LiteralExpression( s: String ) => Environment.varRegex.replaceAllIn( s, replacer )
			case LiteralExpression( v ) => v
			case VariableExpression( _, Some(v) ) => v
			case v@VariableExpression( n, None ) =>
				val o = lookup( n )

				v.value = Some( o )
				o
			case PathParameterExpression( n ) => pathMap( n )
			case QueryParameterExpression( n ) => queryMap get n
			case SystemValueExpression( _, Some(s) ) => s.value
			case s@SystemValueExpression( n, None ) =>
				val v = sbindings( n )

				s.value = Some( v )
				v.value
			case ObjectExpression( pairs ) =>
				Map( pairs map {case (k, v) => (k, deref(v))}: _* )
			case ConditionalExpression( cond, no ) =>
				def condition( ifthen: List[(ExpressionAST, ExpressionAST)] ): Any =
					ifthen match {
						case Nil =>
							no match {
								case None => null
								case Some( e ) => eval( e )
							}
						case (ifexpr, thenexpr) :: tail =>
							if (evalb( ifexpr ))
								eval( thenexpr )
							else
								condition( tail )
					}
					
				condition( cond )
			case ForExpression( gen, body, e ) =>
				def forloop( env: Environment, gs: List[GeneratorAST] ) {
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
			case s@JavaScriptExpression( code, exe ) =>
				if (exe eq null)
					s.exe = jseng.compile( code )

				UtilityFunctions.jsrequest( this )
				s.exe.eval
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

case class Route( method: String, path: URIPath, action: ExpressionAST )

case class Table( name: String, columns: List[Column], columnMap: Map[String, Column], resource: Boolean, mtm: Boolean,
									var preparedInsert: PreparedStatement ) {
	def names = columns map (_.name)
}

case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean, validators: List[ExpressionAST] )

class Variable( var value: Any )

class Enum( val enum: List[String] ) {
	private val map = Map( enum.zipWithIndex: _* )
	private val vec = enum toVector

	def enum2int( s: String ) = map( s )

	def int2enum( i: Int ) = vec( i )
}

class NotFoundException( error: String ) extends Exception( error )

class BadRequestException( error: String ) extends Exception( error )

class ForbiddenException( error: String ) extends Exception( error )

class UnauthorizedException( realm: String ) extends Exception( realm )

class ExpiredException( realm: String ) extends Exception( realm )

class RejectRouteThrowable extends Throwable
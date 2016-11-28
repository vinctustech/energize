package xyz.hyperreal

import java.sql._

import collection.mutable.{ListBuffer, HashMap}
import util.matching.Regex
import util.parsing.input.CharSequenceReader
import collection.mutable.{LinkedHashMap, HashMap, ListBuffer}

import xyz.hyperreal.table.TextTable
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


package object cras {
	
	private val varRegex = "\\$([a-zA-Z][a-zA-Z0-9]*)".r
	
	def dbconnect( dbfile: String, memory: Boolean = false ) = {
		Class.forName( "org.h2.Driver" )
		
		val connection = DriverManager.getConnection( s"jdbc:h2${if (memory) ":mem:" else ":~/"}" + dbfile, "sa", "" )
		
		(connection, connection.createStatement)
	}

	val URI = """(/(?:[a-zA-Z0-9_-]/)*)(?:\?((?:[a-zA-Z]=.*&?)+))?"""r
	
	def process( reqmethod: String, requri: String, reqbody: String, tables: Map[String, Table], routes: List[Route], statement: Statement ): String = {
		val reqpath = requri
		val (vars, expr) =
			find( reqmethod, reqpath, routes ) match {
				case None => return """{"status": "error", "reason": "bad route"}"""
				case Some( ve ) => ve
			}
		
		DefaultJSONWriter.toString( evalj( expr, vars, tables, reqbody, statement ) )
	}
	
	def eval( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ): Any =
		expr match {
			case FunctionExpression( "insert", List(table, json) ) =>
				val t = eval( table, vars, tables, reqbody, statement ).asInstanceOf[Table]
				val j = evalj( json, vars, tables, reqbody, statement )
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
				val t = eval( table, vars, tables, reqbody, statement ).asInstanceOf[Table]
				val j = evalj( json, vars, tables, reqbody, statement )
				val idv = evali( id, vars, tables, reqbody, statement )
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
				val com = evals( sql, vars, tables, reqbody, statement )
				
				Map(
					"status" -> "ok",
					"update" -> statement.executeUpdate( com.toString )
				)
			case FunctionExpression( "query", List(sql) ) =>
				val com = evals( sql, vars, tables, reqbody, statement )
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
	
	def evals( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ): String =
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[String]
	
	def evali( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ): Int =
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[String].toInt
	
	def evalj( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ): Map[String, Any] =
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[Map[String, Any]]
	
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
	
	def configuration( src: io.Source, connection: Connection ): (Map[String, Table], List[Route]) = {
		def problem( error: String ) = {
			sys.error( error )
		}
		
		val statement = connection.createStatement
		val p = new CrasParser
		val ast =
			p.parse( new CharSequenceReader(src.getLines.map(l => l + '\n').mkString) ) match {
				case p.Success( tree, _ ) => tree
				case p.NoSuccess( error, rest ) =>
					println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
					sys.exit
					problem( "" )
			}
		
		val tables = new HashMap[String, LinkedHashMap[String, Column]]
		val routes = new ListBuffer[Route]
		
		ast foreach {
			case TableDefinition( name, bases, columns ) =>
				if (tables contains name)
					problem( "table defined twice: " + name )

				val cols = new LinkedHashMap[String, Column]
				val f =
					columns map {
						case TableColumn( modifiers, typ, cname ) =>
							if (cols contains cname)
								problem( "column defined twice: " + cname )
								
							val t =
								typ match {
									case StringType => "VARCHAR(255)"
									case IntegerType => "INT"
									case UUIDType => "UUID"
									case DateType => "DATE"
								}
							var secret = false
							var required = false
							var optional = false
							var unique = false
							var m = ""
							
							modifiers foreach {
								case UniqueModifier =>
									if (unique)
										problem( "modifier 'unique' encountered more than once: " + name + "/" + cname )
										
									unique = true
									m += " unique"
								case RequiredModifier =>
									if (required)
										problem( "modifier 'required' encountered more than once: " + name + "/" + cname )
										
									if (optional)
										problem( "modifier 'required' encountered along with 'optional': " + name + "/" + cname )
										
									m += " not null"
									required = true
								case OptionalModifier =>
									if (optional)
										problem( "modifier 'optional' encountered more than once: " + name + "/" + cname )
										
									if (required)
										problem( "modifier 'optional' encountered along with 'required': " + name + "/" + cname )
										
									optional = true
								case SecretModifier =>
									if (secret)
										problem( "modifier 'secret' encountered more than once: " + name + "/" + cname )
										
									secret = true	
							}
							
							cols(cname) = Column( cname, typ, secret, required )
							cname + " " + t + m
					} mkString ", "
				
				tables(name) = cols
					
				if (!connection.getMetaData.getTables( null, "PUBLIC", name.toUpperCase, null ).next) {
//					println( "creating table '" + name.toUpperCase + "'" )
					statement.execute( "CREATE TABLE " + name + "(id INT AUTO_INCREMENT PRIMARY KEY, " + f + ")" )
				}
				
				for (URIPath( base ) <- bases) {
					val (_, r) = configuration( io.Source.fromString(
						"""
						|route <base>/<table>
						|  GET    :id    query( "select * from <table> where id = '$id';" )
						|  GET           query( "select * from <table>;" )
						|  POST          insert( <table>, json )
						|  PATCH  :id    update( <table>, json, id )
						|  DELETE :id    command( "delete from <table> where id = '$id';" )
						""".stripMargin.replaceAll("<table>", name).replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString "/")), connection
					)
					
					routes ++= r
				}
					
			case RoutesDefinition( URIPath(base), mappings ) =>
				mappings foreach {
					case URIMapping( GETMethod, URIPath(path), action ) => routes += Route( "GET", base ++ path, action )
					case URIMapping( POSTMethod, URIPath(path), action ) => routes += Route( "POST", base ++ path, action )
					case URIMapping( PUTMethod, URIPath(path), action ) => routes += Route( "PUT", base ++ path, action )
					case URIMapping( PATCHMethod, URIPath(path), action ) => routes += Route( "PATCH", base ++ path, action )
					case URIMapping( DELETEMethod, URIPath(path), action ) => routes += Route( "DELETE", base ++ path, action )
					case _ => problem( "unknown method" )
				}
		}
		
		val tableMap = tables.map {
			case (tname, tinfo) => {
				val cnames = new ListBuffer[String]
				
				tinfo foreach {
					case (cname, cinfo) =>
						cnames += cname
				}
				
				(tname, Table( tname, cnames.toList, tinfo.toMap ))
			}
		} toMap
		
		(tableMap, routes.toList)
	}

	case class Route( method: String, path: List[URISegment], action: ExpressionAST )

	case class Table( name: String, names: List[String], columns: Map[String, Column] )

	case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean )
	
}
package xyz.hyperreal

import java.sql._

import collection.mutable.{ListBuffer, HashMap}
import util.matching.Regex
import util.parsing.input.{Position, CharSequenceReader}
import collection.mutable.{LinkedHashMap, HashMap, ListBuffer}

import xyz.hyperreal.table.TextTable
import xyz.hyperreal.json.{DefaultJSONReader, DefaultJSONWriter}


package object cras {
	
	private val varRegex = "\\$([a-zA-Z][a-zA-Z0-9]*)".r
	
	def dbconnect( dbfile: String, memory: Boolean = false ) = {
		Class.forName( "org.h2.Driver" )
		
		val connection = DriverManager.getConnection( s"jdbc:h2${if (memory) ":mem:" else ":"}" + dbfile, "sa", "" )
		
		(connection, connection.createStatement)
	}

	val URI = """(/(?:[a-zA-Z0-9_-]/)*)(?:\?((?:[a-zA-Z]=.*&?)+))?"""r
	
	def process( reqmethod: String, requri: String, reqbody: String, tables: Map[String, Table], routes: List[Route], statement: Statement ) = {
		val reqpath = requri
		
		find( reqmethod, reqpath, routes ) match {
			case None =>
				None
			case Some( (vars, expr) ) =>
				Some( DefaultJSONWriter.toString(
					try {
						Map("status" -> "ok", "data" -> eval( expr, vars, tables, reqbody, statement ))
					} catch {
						case e: CrasInternalException => Map("status" -> "error", "reason" -> e.getMessage)
					}
				) )
		}
	}
	
	class CrasInternalException( message: String ) extends Exception( message )
	
	def eval( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ): Any =
		expr match {
			case ApplyExpression( "insert", List(table, json) ) =>
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
				statement.executeUpdate( com.toString )
			case ApplyExpression( "update", List(table, json, id, all) ) =>
				val t = eval( table, vars, tables, reqbody, statement ).asInstanceOf[Table]
				val j = evalj( json, vars, tables, reqbody, statement )
				val idv = evali( id, vars, tables, reqbody, statement )
				val allf = evalb( all, vars, tables, reqbody, statement )
				
				if (allf && j.keySet != (t.columns.keySet - "id"))
					throw new CrasInternalException( "update: missing column(s) in PUT request" )
				else {
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
					statement.executeUpdate( com.toString )
				}
			case ApplyExpression( "command", List(sql) ) =>
				val com = evals( sql, vars, tables, reqbody, statement )
				
				statement.executeUpdate( com.toString )
			case ApplyExpression( "query", List(sql) ) =>
				val com = evals( sql, vars, tables, reqbody, statement )
				val res = statement.executeQuery( com )
				val list = new ListBuffer[Map[String, Any]]
				val md = res.getMetaData
				val count = md.getColumnCount
				
				while (res.next)
					list += Map( (for (i <- 1 to count) yield (md.getColumnName(i), res.getObject(i))): _* )
				
				list.toList
			case LiteralExpression( s: String ) =>
				def replacer( m: Regex.Match ): String = {
					vars get m.group(1) match {
						case None => sys.error( "variable not found: " + m.group(1) )
						case Some( s ) => s
					}
				}
	
				varRegex.replaceAllIn( s, replacer _ )
			case LiteralExpression( v ) => v
			case VariableExpression( v ) =>
				v match {
					case "json" =>
						DefaultJSONReader.fromString( reqbody )
					case _ if vars contains v => vars(v)
					case _ if tables contains v => tables(v)
					case _ => sys.error( "variable not found: " + v )
				}
			case ObjectExpression( pairs ) =>
				Map( pairs map {case (k, v) => (k, eval(v, vars, tables, reqbody, statement))}: _* )
			case _ => sys.error( "error evaluating expression" )
		}
	
	def evals( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ) =
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[String]
	
	def evali( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ) =
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[String].toInt
	
	def evalj( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement )=
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[Map[String, Any]]
	
	def evalb( expr: ExpressionAST, vars: Map[String, String], tables: Map[String, Table], reqbody: String, statement: Statement ) =
		eval( expr, vars, tables, reqbody, statement ).asInstanceOf[Boolean]
	
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
			})
				return Some( (vars.toMap, action) )
		}
		
		None
	}
	
	def problem( pos: Position, error: String ) = sys.error( pos.line + ": " + error + "\n" + pos.longString )
	
	def configuration( src: io.Source, connection: Connection ): (Map[String, Table], List[Route]) = {
		val statement = connection.createStatement
		val p = new CrasParser
		val ast =
			p.parse( new CharSequenceReader(src.getLines.map(l => l + '\n').mkString) ) match {
				case p.Success( tree, _ ) => tree
				case p.NoSuccess( error, rest ) =>
					problem( rest.pos, error )
			}
		
		val tables = new HashMap[String, LinkedHashMap[String, Column]]
		val routes = new ListBuffer[Route]
		
		ast foreach {
			case TableDefinition( pos, name, bases, columns ) =>
				if (tables contains name)
					problem( pos, s"table '$name' defined twice" )
				
				val cols = new LinkedHashMap[String, Column]
				val f =
					columns map {
						case TableColumn( pos, modifiers, typ, cname ) =>
							if (cols contains cname)
								problem( pos, s"column '$cname' defined twice" )
								
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
						|  PATCH  :id    update( <table>, json, id, false )
						|  PUT    :id    update( <table>, json, id, true )
						|  DELETE :id    command( "delete from <table> where id = '$id';" )
						|
						|result
						|  ("exception", message) -> {status: "error", message: error}
						|  (_, json)              -> {status: "ok", data: json}
						""".stripMargin.replaceAll("<table>", name).
							replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString "/")), connection
					)
					
					routes ++= r
				}
					
			case RoutesDefinition( URIPath(base), mappings ) =>
				mappings foreach {
					case URIMapping( HTTPMethod(method), URIPath(path), action ) => routes += Route( method, base ++ path, action )
				}
			case ResultsDefinition( func ) =>
				
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
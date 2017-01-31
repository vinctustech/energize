package xyz.hyperreal.energize

import java.sql._
import java.io.PrintWriter

import jline.console.ConsoleReader

import xyz.hyperreal.table.TextTable


object REPLMain extends App {
	
	val reader =
		new ConsoleReader {
			setExpandEvents( false )
			setBellEnabled( false )
			setPrompt( "> " )
		}
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = _
	var stacktrace = false

	s"""
	|Welcome to Energize version $VERSION.
	|Type in expressions to have them evaluated.
	|Type help for more information.
	""".trim.stripMargin.lines foreach println
	println

	var env: Environment = _
	var connection: Connection = _
	var statement: Statement = _
	var db: Database = _
	var name = "H2"
	var driver = "org.h2.Driver"
	var url = "jdbc:h2:mem:"
	var user = "sa"
	var password = ""

		
	sys.addShutdownHook {
		connection.close
	}
	
	def connect {
		val (c, s, d) = Energize.dbconnect( name, driver, url, user, password )
		
		connection = c
		statement = s
		db = d
		env = Environment( Map(), Nil, Builtins.map, connection, statement, db )
		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
	}

	connect
	println

	while ({line = reader.readLine; line != null}) {
		val line1 = line.trim
		val com = line1 split "\\s+" toList
	
		def result( method: String, path: String, json: String ) =
			println( env.process(method, path, json) )

		try {
			com match {
				case List( "config"|"co" ) =>
					name = DATABASE.getString( "name" )
					driver = DATABASE.getString( "driver" )
					url = DATABASE.getString( "url" )
					user = DATABASE.getString( "user" )
					password = DATABASE.getString( "password" )
				case List( "connect"|"c" ) =>
					if (connection ne null)
						connection.close

					connect
				case List( "connect"|"c", u ) =>
					if (connection ne null)
						connection.close

					url = u
					connect
				case List( "db" ) =>
					println( driver, url, user, password )
				case List( "driver"|"d", d ) =>
					driver = d
				case List( "help"|"h" ) =>
					"""
					|config                               set database parameters from config file
					|connect (c)                          (re)connect to database using current database parameters
					|connect (c) <url>                    connect to database using <url> clearing in-memory table and routing information
					|db                                   show current database parameters
					|driver (d) <driver>                  set database <driver>
					|load (l) <config>                    load a <config> (".energize" file) creating all tables and routes as specified
					|help (h)                             print this summary
					|password (p) <password>              set database <password>
					|quit (q)                             exit the REPL
					|routes (r)                           print all routes showing absolute paths
					|stack (s) on/off                     turn exception stack trace on or off
					|user (u) <user>                      set database <user>
					|GET/POST/PUT/DELETE <path> [<json>]  issue a request with optional <json> message body
					|select ...                           execute SQL query
					|<SQL>                                execute <SQL> non-query command
					|?<expression>                        evaluate an ENERGIZE action script expression
					""".trim.stripMargin.lines foreach out.println
				case List( "load"|"l", config ) =>
					env = Energize.configure( io.Source.fromFile(config + ".energize"), connection, statement, db )
//				case List( "wipe"|"w" ) =>
//					connection.close
//					new File( sys.props("user.home"), db + ".mv.db" ).delete
//					new File( sys.props("user.home"), db + ".trace.db" ).delete
//					connect
//					env = null
				case List( "password"|"p", p ) =>
					password = p
				case List( "quit"|"q" ) =>
					if (connection ne null)
						connection.close

					sys.exit
				case List( "routes"|"r" ) =>
					for (Route(method, path, action) <- env.routes ) {
						val pathbuf = new StringBuilder
						
						path foreach {
							case NameURISegment( name ) =>
								pathbuf += '/'
								pathbuf ++= name
							case ParameterURISegment( parm, typ ) =>
								pathbuf += '/'
								pathbuf ++= parm
								pathbuf += ':'
								pathbuf ++= typ
						}
						
						println( method + " " + pathbuf + " " + action )
					}
				case List( "stack"|"s", "on" ) => stacktrace = true
				case List( "stack"|"s", "off" ) => stacktrace = false
				case List( "user"|"u", u ) =>
					user = u
				case Nil|List( "" ) =>
				case List( method@("GET"|"get"|"DELETE"|"delete"), path ) =>
					result( method, path, null )
				case (method@("GET"|"get"|"POST"|"post"|"PUT"|"put"|"PATCH"|"patch")) :: path :: _ =>
					result( method, path, line1.split("\\s+", 3)(2) )
				case "select" :: _ =>
					print( TextTable(statement.executeQuery(line1)) )
				case _ if line1 startsWith "?" =>
					println( env evaluate line1.substring(1) )
				case _ => //sql non-query command
					statement.execute( line1 )
			}
		}
		catch
		{
			case e: Exception =>
				if (stacktrace)
					e.printStackTrace( out )
				else
					out.println( e )
		}
		
		out.println
	}
	
}
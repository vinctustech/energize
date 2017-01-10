package xyz.hyperreal.cras

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
	|Welcome to CRAS version $VERSION.
	|Type in expressions to have them evaluated.
	|Type help for more information.
	|
	""".trim.stripMargin.lines foreach println

	var env: Env = _
	var connection: Connection = _
	var statement: Statement = _
	var driver = "org.h2.Driver"
	var url = "jdbc:h2:mem:"
	var username = "sa"
	var password = ""

		
	sys.addShutdownHook {
		connection.close
	}
	
	def connect {
		val (c, s) = Cras.dbconnect( driver, url, username, password )
		
		connection = c
		statement = s
		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
		println
	}
	
	connect
	
	while ({line = reader.readLine; line != null}) {
		val line1 = line.trim
		val com = line1 split "\\s+" toList
	
		def result( method: String, path: String, json: String ) =
			env.process( method, path, json ) match {
				case None => println( "route not found" )
				case Some( data ) => println( data )
			}
		
		try {
			com match {
				case List( "config"|"co" ) =>
					driver = DATABASE.getString( "driver" )
					url = DATABASE.getString( "url" )
					username = DATABASE.getString( "username" )
					password = DATABASE.getString( "password" )
				case List( "connect"|"c" ) =>
					if (connection ne null)
						connection.close

					connect
					env = null
				case List( "connect"|"c", u ) =>
					if (connection ne null)
						connection.close

					url = u
					connect
					env = null
				case List( "db" ) =>
					println( driver, url, username, password )
				case List( "driver"|"d", d ) =>
					driver = d
				case List( "help"|"h" ) =>
					"""
					|connect (c) <database>               connect to <database> (relative to user folder) clearing in-memory table and routing information
					|load (l) <config>                    load a <config> (".cras" file) creating all tables and routes as specified
					|help (h)                             print this summary
					|quit (q)                             exit the REPL
					|routes (r)                           print all routes showing absolute paths
					|stack (s) on/off                     turn exception stack trace on or off
					|wipe (w)                             wipe current database clean and reconnect
					|GET/POST/PUT/DELETE <path> [<json>]  issue a request with optional <json> message body
					|select ...                           execute SQL query
					|<SQL>                                execute <SQL> non-query command
					""".trim.stripMargin.lines foreach out.println
				case List( "load"|"l", config ) =>
					env = Cras.configure( io.Source.fromFile(config + ".cras"), connection, statement )
				case List( "quit"|"q" ) =>
					connection.close
					sys.exit
				case List( "stack"|"s", "on" ) => stacktrace = true
				case List( "stack"|"s", "off" ) => stacktrace = false
//				case List( "wipe"|"w" ) =>
//					connection.close
//					new File( sys.props("user.home"), db + ".mv.db" ).delete
//					new File( sys.props("user.home"), db + ".trace.db" ).delete
//					connect
//					env = null
				case List( "password"|"p", p ) =>
					password = p
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
				case List( "username"|"u", u ) =>
					username = u
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
package xyz.hyperreal.cras

import java.sql._
import java.io.{PrintWriter, File}

import jline.console.ConsoleReader

import xyz.hyperreal.table.TextTable


object REPLMain extends App {
	
	val reader = new ConsoleReader
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = null
	var stacktrace = false
	
	reader.setBellEnabled( false )
	reader.setPrompt( "> " )

	"""
	|Welcome to CRAS version 0.2.
	|Type in expressions to have them evaluated.
	|Type help for more information.
	""".trim.stripMargin.lines foreach println
	println

	var env: Env = null
	var connection: Connection = null
	var statement: Statement = null
	var db = "./test"
		
	sys.addShutdownHook {
		connection.close
	}
	
	def connect( file: String ) {
		val (c, s) = dbconnect( file )
		
		connection = c
		statement = s
		db = file
		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
		println
	}
	
	connect( db )
	
	while ({line = reader.readLine; line != null}) {
		val line1 = line.trim
		val com = line1 split "\\s+" toList
	
		def result( method: String, path: String, json: String ) =
			process( method, path, json, env ) match {
				case None => println( "route not found" )
				case Some( data ) => println( data )
			}
		
		try {
			com match {
				case List( "connect|c", dbfile ) =>
					if (connection ne null)
						connection.close
						
					connect( dbfile )
					db = dbfile
					env = null
				case List( "help"|"h" ) =>
					"""
					|connect (c) <database>               connect to <database> (relative to user folder) clearing in-memory table and routing information
					|load (l) <config>                    load a <config> (".cras" file) creating all tables and routes as specified
					|help (h)                             print this summary
					|quit (q)                             exit the REPL
					|routes (r)                           print all routes showing absolute paths
					|stack (t) on/off                     turn exception stack trace on or off
					|wipe (w)                             wipe current database clean and reconnect
					|GET/POST/PUT/DELETE <path> [<json>]  issue a request with optional <json> message body
					|select ...                           execute SQL query
					|<SQL>                                execute <SQL> non-query command
					""".trim.stripMargin.lines foreach out.println
				case List( "load"|"l", config ) =>
					env = configure( io.Source.fromFile(config + ".cras"), connection, statement )
				case List( "quit"|"q" ) =>
					connection.close
					sys.exit
				case List( "stack"|"s", "on" ) => stacktrace = true
				case List( "stack"|"s", "off" ) => stacktrace = false
				case List( "wipe"|"w" ) =>
					connection.close
					new File( sys.props("user.home"), db + ".mv.db" ).delete
					new File( sys.props("user.home"), db + ".trace.db" ).delete
					connect( db )
					env = null
				case List( "routes"|"r" ) =>
					for (Route(method, path, action) <- env.routes ) {
						val pathbuf = new StringBuilder
						
						path foreach {
							case NameURISegment( name ) =>
								pathbuf += '/'
								pathbuf ++= name
							case ParameterURISegment( parm ) =>
								pathbuf += '/'
								pathbuf += ':'
								pathbuf ++= parm
						}
						
						println( method + " " + pathbuf + " " + action )
					}
				case Nil|List( "" ) =>
				case (method@("POST"|"post"|"PUT"|"put"|"PATCH"|"patch")) :: path :: _ =>
					result( method, path, line1.split("\\s+", 3)(2) )
				case List( method@("GET"|"get"|"DELETE"|"delete"), path ) =>
					result( method, path, "{}" )
				case "select" :: _ =>
					print( TextTable(statement.executeQuery(line1)) )
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
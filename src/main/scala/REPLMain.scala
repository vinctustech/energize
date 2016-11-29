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
	|Welcome to CRAS version 0.1.
	|Type in expressions to have them evaluated.
	|Type help for more information.
	""".trim.stripMargin.lines foreach println
	println

	var tables: Map[String, Table] = null
	var routes: List[Route] = null
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
		
		try {
			com match {
				case List( "connect|c", dbfile ) =>
					if (connection ne null)
						connection.close
						
					connect( dbfile )
					db = dbfile
					tables = null
					routes = null
				case List( "help"|"h" ) =>
					"""
					|connect (c) <database>               connect to <database> (relative to user folder) clearing in-memory table and routing information
					|load (l) <config>                    load a <config> (".cras" file) creating all tables and routes as specified
					|help (h)                             print this summary
					|quit (q)                             exit the REPL
					|stack (t) on/off                     turn exception stack trace on or off
					|wipe (w)															wipe current database clean and reconnect
					|GET/POST/PUT/DELETE <path> [<json>]  issue a request with optional <json> message body
					|select ...                           execute SQL query
					|<SQL>                                execute <SQL> non-query command
					""".trim.stripMargin.lines foreach out.println
				case List( "load"|"l", config ) =>
					val (t, r) = configuration( io.Source.fromFile(config + ".cras"), connection )

					tables = t
					routes = r
				case List( "quit"|"q" ) =>
					connection.close
					sys.exit
				case List( "stack"|"s", "on" ) => stacktrace = true
				case List( "stack"|"s", "off" ) => stacktrace = false
				case List( "wipe"|"w" ) =>
					statement.close
					new File( sys.props("user.home"), db + ".mv.db" ).delete
					new File( sys.props("user.home"), db + ".trace.db" ).delete
					connect( db )
				case Nil|List( "" ) =>
				case (method@("POST"|"post"|"PUT"|"put"|"PATCH"|"patch")) :: path :: _ =>
					println( process( method, path, line1.split("\\s+", 3)(2), tables, routes, statement ) )
				case List( method@("GET"|"get"|"DELETE"|"delete"), path ) =>
					println( process( method, path, "", tables, routes, statement ) )
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
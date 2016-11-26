package xyz.hyperreal.informatio

import java.io.{PrintWriter, File}

import jline.console.ConsoleReader

import xyz.hyperreal.table.TextTable


object Main extends App {
	
	val reader = new ConsoleReader
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = null
	var stacktrace = false
	
	reader.setBellEnabled( false )
	reader.setPrompt( "> " )

	"""
	|Welcome to Informatio version 0.1.
	|Type in expressions to have them evaluated.
	|Type help for more information.
	""".trim.stripMargin.lines foreach println
	println

	var tables: Map[String, Table] = null
	var routes: List[Route] = null
	
	connect( "projects/informatio/test" )
	println
	
	while ({line = reader.readLine; line != null}) {
		val line1 = line.trim
		val com = line1 split "\\s+" toList
		
		try {
			com match {
				case List( "connect|c", dbfile ) =>
					connect( dbfile )
					tables = null
					routes = null
				case List( "help"|"h" ) =>
					"""
					|connect (c) <database>               connect to <database> (relative to user folder) clearing in-memory table and routing information
					|load (l) <config>                    load a <config> (".info" file) creating all tables and routes as specified
					|help (h)                             print this summary
					|quit (q)                             exit the REPL
					|stack (t) on/off                     turn exception stack trace on or off
					|GET/POST/PUT/DELETE <path> [<json>]  issue a request with optional <json> message body
					|select ...                           execute SQL query
					|<SQL>                                execute <SQL> non-query command
					""".trim.stripMargin.lines foreach out.println
				case List( "load"|"l", config ) =>
					val (t, r) = Interpreter( new File(config + ".info") )

					tables = t
					routes = r
				case List( "quit"|"q" ) =>
					close
					sys.exit
				case List( "stack"|"s", "on" ) => stacktrace = true
				case List( "stack"|"s", "off" ) => stacktrace = false
				case Nil|List( "" ) =>
				case List( method@("POST"|"post"), path, json ) =>
					println( process( method, path, json, tables, routes ) )
				case List( method@("GET"|"get"|"PUT"|"put"|"DELETE"|"delete"), path ) =>
					println( process( method, path, "", tables, routes ) )
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
package xyz.hyperreal.energize

import java.sql.{Connection, Statement}
import java.io.PrintWriter

import collection.mutable.HashMap
import jline.console.ConsoleReader
import org.apache.http.message.{BasicHeader, BasicHeaderValueParser}
import xyz.hyperreal.table.TextTable

object REPLMain extends App {

  val reader =
    new ConsoleReader {
      setExpandEvents(false)
      setBellEnabled(false)
      setPrompt("> ")
    }
  val out =
    new PrintWriter(reader.getTerminal.wrapOutIfNeeded(System.out), true)
  var line: String = _
  var stacktrace = false

  println(s"""
		|Welcome to Energize version $VERSION.
		|Type in expressions to have them evaluated.
		|Type ? for more information.
		""".trim.stripMargin)
  println

  val headers = new HashMap[String, String]
  var pro: Processor = _
  var connection: Connection = _
  var statement: Statement = _
  var db: Database = _
  var name = "H2"
  var driver = "org.h2.Driver"
  var url = "jdbc:h2:mem:"
  var user = "sa"
  var password = ""
  var config: String = _
  var key = AUTHENTICATION.getString("key")

  sys.addShutdownHook {
    connection.close
  }

  def parseHeader(v: String) =
    BasicHeaderValueParser.parseElements(v, BasicHeaderValueParser.INSTANCE)

  def connect {
    val (c, s, d) = Definition.dbconnect(name, driver, url, user, password)

    connection = c
    statement = s
    db = d
    println(connection)
    println(
      connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion)

    if (config eq null) {
      println("loading builtin definition")
      pro = Definition.define("", connection, statement, db, key)
    } else {
      println(s"loading $config definition file")
      pro = Definition.define(io.Source.fromFile(config + ".energize"),
                              connection,
                              statement,
                              db,
                              key)
    }
  }

  def reconnect = {
    if (connection ne null)
      connection.close

    connect
  }

  connect
  println

  while ({ line = reader.readLine; line != null }) {
    val line1 = line.trim

    def rest(count: Int) = line1.split("\\s+", count)(count - 1)

    val com = line1 split "\\s+" toList

    def result(method: String, uri: String, json: String) =
      println(pro.process(method.toUpperCase, uri, null, json, null))

    try {
      com match {
        case List("?") =>
          println(
            """
            |?                                          print this command summary
            |config                                     set database parameters from config file
            |connect|c                                  (re)connect to database using current database parameters
            |connect|c <url>                            connect to database using <url>, clearing in-memory table and routing information
            |db                                         show current database parameters
            |driver|d <driver>                          set database <driver>
            |eval|e <expression>                        evaluate an Energize action script expression
						|header|h <header> <value>                  set header <name> to <value>
						|header|h <header>                          delete header <name>
						|header|h                                   show headers
            |load|l                                     reload previously loaded configuration
            |load|l <config>                            load a <config> (".energize" file), creating all tables and routes as specified
            |password|p <password>                      set database <password>
						|quit|q                                     exit the REPL
            |routes|r                                   print all routes showing absolute paths
            |trace|t on|off                             turn exception stack trace on or off
            |user|u <user>                              set database <user>
            |GET|POST|PUT|PATCH|DELETE <path> [<json>]  issue a request with optional <json> message body
            |select ...                                 execute SQL query
            |<SQL>                                      execute <SQL> non-query command
          """.trim.stripMargin)
        case List("config" | "co") =>
          name = DATABASE.getString("name")
          driver = DATABASE.getString("driver")
          url = DATABASE.getString("url")
          user = DATABASE.getString("user")
          password = DATABASE.getString("password")
        case List("connect" | "c") =>
          reconnect
        case List("connect" | "c", u) =>
          if (connection ne null)
            connection.close

          url = u
          connect
        case List("db") =>
          println(
            s"driver: $driver, url: $url, user: $user, password: $password")
        case List("driver" | "d", d) =>
          driver = d
        case List("header" | "h") =>
          if (headers nonEmpty) println(headers mkString "\n")
        case List("header" | "h", h) =>
          headers remove h match {
            case None    => println(s"header $h wasn't set")
            case Some(_) => println(s"header $h removed")
          }
        case ("header" | "h") :: h :: v if v != Nil =>
          headers(h) = rest(3)
          println(Message.elements(new BasicHeader(h, rest(3))) mkString "\n")
        case List("load" | "l") =>
          if (config eq null)
            println("no configuration has been loaded")

          reconnect
        case List("load" | "l", conf) =>
          config = conf
          reconnect

//				case List( "wipe"|"w" ) =>
//					connection.close
//					new File( sys.props("user.home"), db + ".mv.db" ).delete
//					new File( sys.props("user.home"), db + ".trace.db" ).delete
//					connect
//					env = null
        case List("password" | "p", p) =>
          password = p
        case List("quit" | "q") =>
          if (connection ne null) connection.close
          sys.exit
        case List("routes" | "r") =>
          for (Route(method, path, _, _, action) <- pro.routes)
            println(s"$method ${path2string(path)} => ${Unparse(action)}")
        case List("trace" | "t", "on")  => stacktrace = true
        case List("trace" | "t", "off") => stacktrace = false
        case List("user" | "u", u)      => user = u
        case Nil | List("")             =>
        case List(method @ ("GET" | "get" | "DELETE" | "delete"), path) =>
          result(method, path, null)
        case (method @ ("GET" | "get" | "POST" | "post" | "PUT" | "put" |
            "PATCH" | "patch")) :: path :: a if a != Nil =>
          result(method, path, rest(3))
        case "select" :: _ =>
          print(TextTable(statement.executeQuery(line1)))
//				case _ if line1 startsWith "?" =>
//					println( env evaluate line1.substring(1) )
        case _ => //sql non-query command
          statement.execute(line1)
      }
    } catch {
      case e: Exception =>
        if (stacktrace)
          e.printStackTrace(out)
        else
          out.println(e)
    }

    out.println
  }

}

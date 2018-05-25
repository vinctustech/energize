//@
package xyz.hyperreal

import java.nio.charset.Charset
import java.time.{Instant, ZoneOffset}
import java.util.Base64

import scala.util.parsing.input.Position
import com.typesafe.config.ConfigFactory
import xyz.hyperreal.bvm.{ExpressionAST, VM}


package object energize {

	type OBJ = Map[String, AnyRef]

	lazy val CONFIG = ConfigFactory.load
	lazy val DATABASE = CONFIG.getConfig( "database" )
	lazy val SERVER = CONFIG.getConfig( "server" )
  lazy val CHARSET = Charset forName SERVER.getString( "charset" )
	lazy val AUTHENTICATION = CONFIG.getConfig( "authentication" )
	lazy val ADMIN = CONFIG.getConfig( "admin" )
	lazy val VERSION = SERVER.getString("version")

	val EXTENSION = ".*\\.(.*)"r

	private val hex = "0123456789ABCDEF"

	implicit def vm2proc( vm: VM ): Processor = vm.args.asInstanceOf[Processor]

	def now = Instant.now.atOffset( ZoneOffset.UTC )

	def nameIn( n: String ) = n + '_'

	def nameOut( n: String ) = n.substring( 0, n.length - 1 )

	def idIn = nameIn( "_id" )

	def escapeQuotes( s: String ): String = s replace ("'", "''")

	def escapeQuotes( json: OBJ ): OBJ =
		json map {case (k, v) =>
			(k, v match {
				case s: String => escapeQuotes( s )
				case _ => v
			})
		}

	def parseExpression( expression: String ): ExpressionAST = {
		val p = new EnergizeParser

		p.parseFromString( expression, p.expressionStatement )
	}

	//	def parseStatements( statements: String ) = {
	//		val p = new EnergizeParser
	//
	//		p.parseFromString( statements, p.statements )
	//	}

	def hex2array( s: String ) = s grouped 2 map (Integer.parseInt(_, 16).toByte) toList

	def byte2hex( b: Byte ) = new String( Array(hex charAt ((b&0xFF) >> 4), hex charAt (b&0x0F)) )

	def bytes2hex( a: Array[Byte] ) = a map byte2hex mkString

	def bytes2base64( data: Array[Byte] ) = new String( Base64.getMimeEncoder.encode(data) )

	def base642bytes( data: String ) = Base64.getMimeDecoder.decode( data.getBytes )

	def problem( pos: Position, error: String ) =
		if (pos eq null)
			sys.error( error )
		else
			sys.error( pos.line + ": " + error + "\n" + pos.longString )

	def integer( s: String ) = s.forall( _.isDigit )

	def path2string( path: PathSegment ): String =
		path match {
			case ConcatenationPathSegment( segments ) => segments map path2string mkString
			case LiteralPathSegment( s ) => s
			case TypePathSegment( t ) => t
			case RemainingPathSegment => "..."
			case SlashPathSegment => "/"
			case AlternationPathSegment( segments ) => s"""(${segments map path2string mkString "|"})"""
			case GroupedPathSegment( s ) => s"($s)"
			case ParameterPathSegment( _, param, None ) => s"$param:"
			case ParameterPathSegment( _, param, Some(p) ) => s"$param: ${path2string( p )}"
			case ZeroOrMorePathSegment => "*"
			case EmptyPathSegment => ""
			case OptionalPathSegment( p ) => path2string( p ) + "?"
			case OneOrMorePathSegment( p ) => path2string( p ) + "+"
			case RepeatPathSegment( subpat, lower, _, upper ) => s"${path2string( subpat )}{$lower, $upper}"
		}

	def contentTypeFromExtension( filename: String ) =
		filename.toLowerCase match {
			case EXTENSION(ext) =>
				ext match {
					case "html"|"htm" => "text/html"
          case "xml" => "text/xml"
					case "jpeg"|"jpg" => "image/jpeg"
					case "css" => "text/css"
					case "js" => "application/javascript"
					case "json" => "application/json"
					case "md"|"sbt"|"scala"|"txt"|"yml" => "text/plain"
					case _ => "application/octet-stream"
				}
			case _ => "application/octet-stream"
		}

//	def dropPostgresTables( connection: Connection, statement: Statement, db: Database ): Unit = {
//		if (db == PostgresDatabase) {
//			val rs = connection.getMetaData.getTables( null, db.publicSchema, null, null )
//
//			while (rs.next) {
//				val t = rs.getString( 3 )
//
//				if (!(t matches ".*(__seq|_pkey|__key|__idx)"))
//					statement.execute( s"drop table $t cascade" )
//			}
//		}
//	}

}

//def problem( pos: Int, input: ParserInput, msg: String ): Unit = {
//	val p = Position( pos, input )
//	val pe = ParseError( p, p, null )
//	import p._
//	val ef = new ErrorFormatter( expandTabs = 4 )
//	val (expandedCol, _) = ef.expandErrorLineTabs( input getLine line, column )
//
//	println( s"$msg (line $line, column $expandedCol):" )
//	println( ef.formatErrorLine( pe, input ) )
//}

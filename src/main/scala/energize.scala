package xyz.hyperreal

import java.util.Base64

import com.typesafe.config.ConfigFactory

import util.parsing.input.Position


package object energize {
	
	type OBJ = Map[String, AnyRef]

	lazy val VERSION = "0.11"
	lazy val CONFIG = ConfigFactory.load
	lazy val DATABASE = CONFIG.getConfig( "database" )
	lazy val SERVER = CONFIG.getConfig( "server" )
	lazy val AUTHORIZATION = CONFIG.getConfig( "authorization" )
	lazy val DATETIME = CONFIG.getConfig( "datetime" )
	lazy val ADMIN = CONFIG.getConfig( "admin" )

	private val hex = "0123456789ABCDEF"

	def nameIn( n: String ) = n + '_'

	def nameOut( n: String ) = n.substring( 0, n.length - 1 )

	def idIn = nameIn( "id" )

	def problem( pos: Position, error: String ) =
		if (pos eq null)
			sys.error( error )
		else
			sys.error( pos.line + ": " + error + "\n" + pos.longString )
	
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

		p.parseFromString( expression, p.expressionStatement ).expr
	}

	def parseStatements( statements: String ) = {
		val p = new EnergizeParser

		p.parseFromString( statements, p.statements )
	}

	def hex2array( s: String ) = s grouped 2 map (Integer.parseInt(_, 16).toByte) toList

	def byte2hex( b: Byte ) = new String( Array(hex charAt ((b&0xFF) >> 4), hex charAt (b&0x0F)) )

	def bytes2hex( a: Array[Byte] ) = a map byte2hex mkString

	def bytes2base64( data: Array[Byte] ) = new String( Base64.getMimeEncoder.encode(data) )

	def base642bytes( data: String ) = Base64.getMimeDecoder.decode( data.getBytes )

//	def pbase642bytes( data: String ) = {
//		val res = base642bytes( data )
//
//		println( bytes2hex(res) )
//		res
//	}
}
package xyz.hyperreal

import java.util.Base64

import com.typesafe.config.ConfigFactory

import util.parsing.input.Position


package object energize {
	
	type OBJ = Map[String, AnyRef]

	lazy val VERSION = "0.8"
	lazy val CONFIG = ConfigFactory.load
	lazy val DATABASE = CONFIG.getConfig( "database" )
	lazy val SERVER = CONFIG.getConfig( "server" )
	lazy val AUTHORIZATION = CONFIG.getConfig( "authorization" )
	lazy val DATETIME = CONFIG.getConfig( "datetime" )
	lazy val ADMIN = CONFIG.getConfig( "admin" )

	private val hex = "0123456789ABCDEF"

	def problem( pos: Position, error: String ) = sys.error( pos.line + ": " + error + "\n" + pos.longString )
	
	def escapeQuotes( s: String ): String = s replace ("'", "''")
		
	def escapeQuotes( json: OBJ ): OBJ =
		json map {case (k, v) =>
			(k, v match {
				case s: String => escapeQuotes( s )
				case _ => v
			})
		}

	def parseCode( code: String ) = {
		val p = new EnergizeParser

		p.parseFromString( code, p.statements )
	}

	def byte2hex( b: Byte ) = new String( Array(hex charAt (b >> 4), hex charAt (b&0x0F)) )

	def bytes2base64( data: Array[Byte] ) = new String( Base64.getMimeEncoder.encode(data) )

	def base642bytes( data: String ) = Base64.getMimeDecoder.decode( data.getBytes )
}
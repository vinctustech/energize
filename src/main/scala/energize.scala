package xyz.hyperreal

import com.typesafe.config.ConfigFactory

import util.parsing.input.Position


package object energize {
	
	type OBJ = Map[String, AnyRef]

	lazy val VERSION = "0.6"
	lazy val CONFIG = ConfigFactory.load
	lazy val DATABASE = CONFIG.getConfig( "database" )
	lazy val SERVER = CONFIG.getConfig( "server" )

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
}
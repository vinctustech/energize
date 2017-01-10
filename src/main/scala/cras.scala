package xyz.hyperreal

import com.typesafe.config.ConfigFactory

import util.parsing.input.Position


package object cras {
	
//	type JSON = Map[String, Any]

	lazy val VERSION = "0.5"
	lazy val CONFIG = ConfigFactory.load
	lazy val DATABASE = CONFIG.getConfig( "database" )
	lazy val SERVER = CONFIG.getConfig( "server" )

	def problem( pos: Position, error: String ) = sys.error( pos.line + ": " + error + "\n" + pos.longString )
	
	def escapeQuotes( s: String ): String = s replace ("'", "''")
		
	def escapeQuotes( json: Map[String, Any] ): Map[String, Any] =
		json map {case (k, v) =>
			(k, if (v.isInstanceOf[String]) escapeQuotes( v.asInstanceOf[String] ) else v)
		}
}
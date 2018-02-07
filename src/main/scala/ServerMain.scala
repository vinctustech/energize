package xyz.hyperreal.energize2

import xyz.hyperreal.options._


object ServerMain extends App {

	val options = new Options( List("-j", "-c", "--"), Nil, Nil )
	val arg = options parse args

	if (!options.set( "--" ) && arg.length != 1 || options.set( "--" ) && arg.nonEmpty) {
		s"""
			|Energize2 v0.1
			|Usage: java -jar energize2-0.1.jar [-j -c] <config>
			|  where <config> is either a file name (without .energize),
			|                           a configuration string (if -c was used), or
			|                           -- to read the configuration from standard input
		""".trim.stripMargin.lines foreach println

		sys.exit( 1 )
	}

	val json = options set "-j"
	val config =
		if (options set "-c")
			io.Source.fromString( arg.head )
		else if (options set "--")
			io.Source.stdin
		else
			io.Source.fromFile( arg.head + (if (json) ".json" else ".energize") )

	EnergizeServer.instance( config, json, SERVER.getInt("port") )
}
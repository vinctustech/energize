package xyz.hyperreal.energize

import xyz.hyperreal.options._


object ServerMain extends App {

	val options = new Options( List("-j", "-c", "--"), Nil, Nil )
	val arg = options parse args

	if (!options.set( "--" ) && arg.length != 1 || options.set( "--" ) && arg.nonEmpty) {
		"""
			|Energize version $VERSION.
			|Usage: java -jar energize-0.8.jar [-j -c] <config>
			|  where <config> is either a file name (without .energize),
			|                           a configuration string (is -c was used), or
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

	instance( config, json, SERVER.getInt("port") )

	def instance( config: io.Source, json: Boolean, port: Int ): Unit = {
		val (connection, statement, db) = Energize.dbconnect

		sys.addShutdownHook {
			connection.close
		}

		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )

		val env =
			if (json)
				Energize.configureFromJSON( config, connection, statement, db )
			else
				Energize.configure( config, connection, statement, db )

		println( "starting server on port " + port )
		new EnergizeServer( env, port ).start
	}
}
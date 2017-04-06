package xyz.hyperreal.energize

import java.sql.{Connection, Statement}


object ServerMain extends App {
	
	if (args.length < 1) {
		println( "usage: java -jar energize-0.8.jar <config>" )
		sys.exit( 1 )
	}
	
	val config = io.Source.fromFile( args(0) + ".energize" )
	val port = SERVER.getInt("port")
	val (connection, statement, db) = Energize.dbconnect
	
	instance( config, port, connection, statement, db )

	def instance( config: io.Source, port: Int, connection: Connection, statement: Statement, db: Database ): Unit = {
		sys.addShutdownHook {
			connection.close
		}

		println( connection )
		println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )

		val env = Energize.configure( config, connection, statement, db )

		println( "starting server on port " + port )
		new Server( env, port ).start
	}
}
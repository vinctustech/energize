package xyz.hyperreal.cras

import java.io.File


object ServerMain extends App {
	
	if (args.length < 1) {
		println( "usage: java -cp cras-0.3.jar xyz.hyperreal.cras.ServerMain <config>" )
		sys.exit( 1 )
	}
	
	val config = args(0) + ".cras"
	val (connection, statement) = Cras.dbconnect
	
	sys.addShutdownHook {
		connection.close
	}
	
	println( connection )
	println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
	println( "loading " + config )

	val env = Cras.configure( io.Source.fromFile(config), connection, statement )
	
	println( "starting server" )
	new Server( 8080, env ).start
}
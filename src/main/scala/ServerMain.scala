package xyz.hyperreal.cras

import java.io.File


object ServerMain extends App {
	
	if (args.length < 2) {
		println( "usage: java -cp cras-0.1.jar xyz.hyperreal.cras.ServerMain <db> <config>" )
		sys.exit( 1 )
	}
	
	val db = args(0)
	val config = args(1) + ".info"
	val (connection, statement) = dbconnect( db )
	
	sys.addShutdownHook {
		connection.close
	}
	
	println( connection )
	println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
	println( "loading " + config )

	val (tables, routes) = configuration( io.Source.fromFile(config), connection )
	
	println( "starting server" )
	new Server( 8080, tables, routes, statement ).start
}
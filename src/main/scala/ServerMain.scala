package xyz.hyperreal.cras


object ServerMain extends App {
	
	if (args.length < 1) {
		println( "usage: java -jar cras-0.5.jar <config>" )
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
	
	println( "starting server on port " + SERVER.getInt("port") )
	new Server( env ).start
}
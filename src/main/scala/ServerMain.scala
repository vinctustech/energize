package xyz.hyperreal.energize


object ServerMain extends App {
	
	if (args.length < 1) {
		println( "usage: java -jar energize-0.6.jar <config>" )
		sys.exit( 1 )
	}
	
	val config = args(0) + ".energize"
	val (connection, statement, db) = Energize.dbconnect
	
	sys.addShutdownHook {
		connection.close
	}
	
	println( connection )
	println( connection.getMetaData.getDriverName + " " + connection.getMetaData.getDriverVersion )
	println( "loading " + config )

	val env = Energize.configure( io.Source.fromFile(config), connection, statement, db )
	
	println( "starting server on port " + SERVER.getInt("port") )
	new Server( env ).start
}
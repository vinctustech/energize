package xyz.hyperreal.cras


object ServerMain extends App {
	
	if (args isEmpty)
		println( "expected config file" )
	
	println( args toList )
	
}
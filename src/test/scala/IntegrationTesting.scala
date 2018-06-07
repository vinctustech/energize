//@
//package xyz.hyperreal.energize

//import scala.sys.process._
//
//
//object IntegrationTesting extends App {
//
//	val tests =
//		List( (
//			"java -jar target/scala-2.12/energize-0.13_snapshot_9.5.jar examples/todo",
//			List(
//				"sh todo-post",
//				"sh todo-get"
//			) ) )
//
//
//	println(
//		Process("ls").lineStream_! mkString "\n"
//	)
//
//}
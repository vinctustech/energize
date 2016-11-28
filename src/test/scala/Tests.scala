package xyz.hyperreal.cras

import org.scalatest._
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with Matchers {
	
	"empty database" in {
		connect( "test", true )

		val config =
			"""
			|table toDos api/v1
			|	name string required
			|	description string optional
			|	status integer required
			""".trim.stripMargin
			
		val (tables, routes) = Interpreter( config )

		process( "GET", "/api/v1/toDos", "", tables, routes ) shouldBe
			"""
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin
		
		close
	}
	
}
package xyz.hyperreal.cras

import org.scalatest._
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with Matchers {
	
	"empty database" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|table toDos api/v1
			|	name string required
			|	description string optional
			|	status integer required
			""".trim.stripMargin
			
		val (tables, routes) = configuration( io.Source.fromString(config), c )

		process( "GET", "/api/v1/toDos", "", tables, routes, s ) shouldBe
			"""
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin
		
		c.close
	}
	
}
package xyz.hyperreal.cras

import org.scalatest._
import prop.PropertyChecks


class ProcessTests extends FreeSpec with PropertyChecks with Matchers {
	
	"empty database" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource todo /api/v1
			|  name        string  required
			|  description string  optional
			|  status      integer required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		process( "GET", "/api/v1/todo", null, env ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
		process( "GET", "/api/v1/todo/1", null, env ) shouldBe None
		process( "GET", "/api/v1/tod", null, env ) shouldBe None
		c.close
	}
	
	"post/get one item" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource todo /api/v1
			|	name        string  required
			|	description string  optional
			|	status      integer required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		process( "POST", "/api/v1/todo", """{"name": "do something", "status": 1}""", env ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": 1
			|}
			""".trim.stripMargin )
		process( "GET", "/api/v1/todo", null, env ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": [
			|    {
			|      "ID": 1,
			|      "name": "do something",
			|      "description": null,
			|      "status": 1
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		process( "GET", "/api/v1/todo/1", null, env ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": {
			|    "ID": 1,
			|    "name": "do something",
			|    "description": null,
			|    "status": 1
			|  }
			|}
			""".trim.stripMargin )
		c.close
	}
	
	"functions/evaluation" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|def f( x, y ) = {"a": x, "b": y, "sum": x + y}
			|
			|route
			|	GET   /f/:a/:b           OK( f(int(a), int(b)) )
			|	GET   /plus/:a/:b        OK( a + b )
			|	GET   /combine           OK( {"a": 1} + json )
			|	GET   /eval              OK( eval(json.expr) )			# GET /eval {"expr": "3 + 4"}
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		process( "GET", "/eval", """ {"expr": "(i + 2)/2*i"} """, env ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": -1/2+i
			|}
			""".trim.stripMargin )
		c.close
	}
}
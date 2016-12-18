package xyz.hyperreal.cras

import org.scalatest._
import prop.PropertyChecks


class processTests extends FreeSpec with PropertyChecks with Matchers {
	
	"empty database" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource todo /api/v1
			|  name        string  required
			|  description string  optional
			|  status      integer required
			|
			|resource test /api/v1
			|  asdf        integer required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		env.process( "GET", "/api/v1/todo", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/api/v1/todo/1", null ) shouldBe None
 		env.process( "GET", "/api/v1/test/1", null ) shouldBe None
 		env.process( "GET", "/api/v1/tod", null ) shouldBe None
		c.close
	}
	
	"empty database (no base)" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource todo
			|  name        string  required
			|  description string  optional
			|  status      integer required
			|
			|resource test
			|  asdf        integer required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		env.process( "GET", "/todo", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/test", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/todo/1", null ) shouldBe None
 		env.process( "GET", "/test/1", null ) shouldBe None
 		env.process( "GET", "/tod", null ) shouldBe None
		c.close
	}
	
	"post/get/delete" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource todo /api/v1
			|	name        string  required
			|	description string  optional
			|	status      integer required
			|
			|resource test /api/v1
			|  asdf       integer required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		env.process( "POST", "/api/v1/todo", """{"name": "do something", "status": 1}""" ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": 1
			|}
			""".trim.stripMargin )

		env.process( "POST", "/api/v1/test", """{"asdf": 123}""" ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": [
			|    {
			|      "id": 1,
			|      "name": "do something",
			|      "description": null,
			|      "status": 1
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": {
			|    "id": 1,
			|    "name": "do something",
			|    "description": null,
			|    "status": 1
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/api/v1/todo/1", null ) shouldBe Some( null )
 		env.process( "GET", "/api/v1/todo/1", null ) shouldBe None
		env.process( "GET", "/api/v1/test", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": [
			|    {
			|      "id": 1,
			|      "asdf": 123
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": {
			|    "id": 1,
			|    "asdf": 123
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/api/v1/test/1", null ) shouldBe Some( null )
 		env.process( "GET", "/api/v1/test/1", null ) shouldBe None
		c.close
	}
	
	"functions/evaluation" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|def f( x, y ) = {"a": x, "b": y, "sum": x + y}
			|
			|route
			|	GET   /f/a:integer/b:integer dataResult( null, f(a, b) )
			|	GET   /plus/a:/b:            dataResult( null, a + b )
			|	GET   /combine               dataResult( null, {"a": 1} + json )
			|	GET   /eval                  dataResult( null, str(eval(json.expr)) )			# GET /eval {"expr": "3 + 4"}
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		env.process( "GET", "/eval", """ {"expr": "(i + 2)/2*i"} """ ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": "-1/2+i"
			|}
			""".trim.stripMargin )
		env.process( "GET", "/f/3/4", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": {
			|    "a": 3,
			|    "b": 4,
			|    "sum": 7
			|  }
			|}
			""".trim.stripMargin )
		c.close
	}
	
	"empty database (one-to-many)" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource users
			|  email       string  unique required
			|  role        roles   required
			|
			|resource roles
			|  type        string  unique required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		env.process( "GET", "/users", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/roles", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/users/1", null ) shouldBe None
 		env.process( "GET", "/roles/1", null ) shouldBe None
 		env.process( "GET", "/user", null ) shouldBe None //deliberatly misspelled
		c.close
	}
	
	"post/get/delete (one-to-many)" in {
		val (c, s) = dbconnect( "test", true )
		val config =
			"""
			|resource users
			|  email       string  unique required
			|  role        roles   required
			|
			|resource roles
			|  type        string  unique required
			""".trim.stripMargin
		val env = configure( io.Source.fromString(config), c, s )

		env.process( "POST", "/roles", """{"type": "normal"}""" ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": 1
			|}
			""".trim.stripMargin )

		env.process( "POST", "/users", """{"email": "joe@blow.com", "role": "normal"}""" ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "GET", "/users", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": [
			|    {
			|      "id": 1,
			|      "email": "joe@blow.com",
			|      "role": {
			|        "id": 1,
			|        "type": "normal"
			|      }
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/users/1", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": {
			|    "id": 1,
			|    "email": "joe@blow.com",
			|    "role": {
			|      "id": 1,
			|      "type": "normal"
			|    }
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/users/1", null ) shouldBe Some( null )
 		env.process( "GET", "/users/1", null ) shouldBe None
		env.process( "GET", "/roles", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": [
			|    {
			|      "id": 1,
			|      "type": "normal"
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/roles/1", null ) shouldBe
			Some( """
			|{
			|  "status": "ok",
			|  "data": {
			|    "id": 1,
			|    "type": "normal"
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/roles/1", null ) shouldBe Some( null )
 		env.process( "GET", "/roles/1", null ) shouldBe None
		c.close
	}
}
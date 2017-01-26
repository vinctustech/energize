package xyz.hyperreal.energize

import org.scalatest._
import prop.PropertyChecks

import org.apache.http.HttpStatus._


class ProcessTests extends FreeSpec with PropertyChecks with Matchers {
	
	"empty database" in {
		val (c, s, d) = Test.dbconnect
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
		val env = Energize.configure( io.Source.fromString(config), c, s, d )

		env.process( "GET", "/api/v1/todo", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test", null ) shouldBe
			(SC_OK,
				"""
					|{
					|  "data": []
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/api/v1/tod", null ) shouldBe
			(SC_NOT_FOUND,
				"""{
					|  "error": "route not found"
					|}""".stripMargin)
		c.close
	}
	
	"empty database (no base)" in {
		val (c, s, d) = Test.dbconnect
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
		val env = Energize.configure( io.Source.fromString(config), c, s, d )

		env.process( "GET", "/todo", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/test", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/todo/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/test/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/tod", null ) shouldBe
			(SC_NOT_FOUND,
				"""{
					|  "error": "route not found"
					|}""".stripMargin)
		c.close
	}
	
	"post/get/delete" in {
		val (c, s, d) = Test.dbconnect
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
		val env = Energize.configure( io.Source.fromString(config), c, s, d )

		env.process( "POST", "/api/v1/todo", """{"name": "do something", "status": 1}""" ) shouldBe
			(SC_CREATED, """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )

		env.process( "POST", "/api/v1/test", """{"asdf": 123}""" ) shouldBe
			(SC_CREATED, """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo", null ) shouldBe
			(SC_OK, """
			|{
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
			(SC_OK, """
			|{
			|  "data": {
			|    "id": 1,
			|    "name": "do something",
			|    "description": null,
			|    "status": 1
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/api/v1/todo/1", null ) shouldBe (SC_NO_CONTENT, null)
 		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/api/v1/test", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "asdf": 123
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": {
			|    "id": 1,
			|    "asdf": 123
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/api/v1/test/1", null ) shouldBe (SC_NO_CONTENT, null)
 		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		c.close
	}
	
	"functions/evaluation" in {
		val (c, s, d) = Test.dbconnect
		val config =
			"""
			|def f( x, y ) = {"a": x, "b": y, "sum": x + y}
			|
			|route
			|	GET   /f/a:integer/b:integer Ok( null, f(a, b) )
			|	GET   /plus/a:/b:            Ok( null, a + b )
			|	GET   /combine               Ok( null, {"a": 1} + json )
			|	GET   /eval                  Ok( null, toString(eval(json.expr)) )			# GET /eval {"expr": "3 + 4"}
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d )

		env.process( "GET", "/eval", """ {"expr": "(i + 2)/2*i"} """ ) shouldBe
			(SC_OK, """
			|{
			|  "data": "-1/2+i"
			|}
			""".trim.stripMargin )
		env.process( "GET", "/f/3/4", null ) shouldBe
			(SC_OK, """
			|{
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
		val (c, s, d) = Test.dbconnect
		val config =
			"""
			|resource users
			|  email       string  unique required
			|  role        roles   required
			|
			|resource roles
			|  type        string  unique required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d )

		env.process( "GET", "/users", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/roles", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/users/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/roles/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/user", null ) shouldBe //deliberatly misspelled
			(SC_NOT_FOUND,
				"""{
					|  "error": "route not found"
					|}""".stripMargin )
		c.close
	}
	
	"post/get/delete (one-to-many)" in {
		val (c, s, d) = Test.dbconnect
		val config =
			"""
			|resource users
			|  email       string  unique required
			|  role        roles   required
			|
			|resource roles
			|  type        string  unique required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d )

		env.process( "POST", "/roles", """{"type": "normal"}""" ) shouldBe
			(SC_CREATED, """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )

		env.process( "POST", "/users", """{"email": "john@doe.com", "role": "normal"}""" ) shouldBe
			(SC_CREATED,  """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "GET", "/users", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "email": "john@doe.com",
			|      "role": {
			|        "id": 1,
			|        "type": "normal"
			|      }
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/users/1", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": {
			|    "id": 1,
			|    "email": "john@doe.com",
			|    "role": {
			|      "id": 1,
			|      "type": "normal"
			|    }
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/users/1", null ) shouldBe (SC_NO_CONTENT, null)
 		env.process( "GET", "/users/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/roles", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "type": "normal"
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/roles/1", null ) shouldBe
			(SC_OK, """
			|{
			|  "data": {
			|    "id": 1,
			|    "type": "normal"
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/roles/1", null ) shouldBe (SC_NO_CONTENT, null)
 		env.process( "GET", "/roles/1", null ) shouldBe
			(SC_NOT_FOUND,
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		c.close
	}
}
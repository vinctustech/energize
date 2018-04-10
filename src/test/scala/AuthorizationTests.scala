package xyz.hyperreal.energize

import org.apache.http.HttpStatus.{SC_CREATED, SC_NO_CONTENT, SC_OK, SC_UNAUTHORIZED}
import org.scalatest._
import prop.PropertyChecks


class AuthorizationTests extends FreeSpec with PropertyChecks with Matchers {

	// todo: add tests for all levels of protection
	"protected resource" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource r protected
				|  f string
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "GET", "/r", new SimpleMessage("Host" -> "example.com:80"), null, null ) shouldBe
			(SC_UNAUTHORIZED, "application/json",
				"""
					|{
					|  "error": "no web token in request"
					|}
				""".trim.stripMargin )
		pro.process( "POST", "/auth/login", new SimpleMessage("Host" -> "example.com:80"), """{"email": "admin@example.com", "password": "password"}""", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MjM0MDQ5MzIsImlhdCI6MTUyMzMxODUzMiwidXNlciI6MX0.EUHIKSAXGdIKdPFjPpCL7QB_H3c5VqkmdSwk0z2wmvc"
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r", new SimpleMessage("Host" -> "example.com:80", "Authorization" -> """Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MjM0MDQ5MzIsImlhdCI6MTUyMzMxODUzMiwidXNlciI6MX0.EUHIKSAXGdIKdPFjPpCL7QB_H3c5VqkmdSwk0z2wmvc"""), null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": []
					|}
				""".trim.stripMargin )
		pro.process( "POST", "/auth/register", new SimpleMessage("Host" -> "example.com:80"), """{"email": "someone@example.com", "password": "someonespassword"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MjM0MDQ5MzIsImlhdCI6MTUyMzMxODUzMiwidXNlciI6Mn0.2SBKOplbUQgaVppu9vyRVYp8ex1uOjRD8WhMqt16N3I"
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r", new SimpleMessage("Host" -> "example.com:80", "Authorization" -> """Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJleHAiOjE1MjM0MDQ5MzIsImlhdCI6MTUyMzMxODUzMiwidXNlciI6Mn0.2SBKOplbUQgaVppu9vyRVYp8ex1uOjRD8WhMqt16N3I"""), null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": []
					|}
				""".trim.stripMargin )

		c.close
	}

}
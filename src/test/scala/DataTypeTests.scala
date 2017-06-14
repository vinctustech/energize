package xyz.hyperreal.energize

import org.scalatest._
import prop.PropertyChecks

import org.apache.http.HttpStatus._


class DataTypeTests extends FreeSpec with PropertyChecks with Matchers {

	"boolean" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource truths
				|  statement   string  required
				|  veracity    boolean required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "POST", "/truths", """{statement: "crackers love cheese", veracity: true}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				 """.trim.stripMargin )
		env.process( "POST", "/truths", """{statement: "we only have five senses", veracity: false}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		env.process( "GET", "/truths", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "statement": "crackers love cheese",
					|      "veracity": true
					|    },
					|    {
					|      "id": 2,
					|      "statement": "we only have five senses",
					|      "veracity": false
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		env.process( "PUT", "/truths/2", """{statement: "we only have 5 senses", veracity: false}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/truths", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "statement": "crackers love cheese",
					|      "veracity": true
					|    },
					|    {
					|      "id": 2,
					|      "statement": "we only have 5 senses",
					|      "veracity": false
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

}

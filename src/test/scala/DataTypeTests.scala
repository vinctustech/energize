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

	"enum" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|enum suit (clubs, diamonds, hearts, spades)
				|
				|resource cards
				|	suit suit
				|	number integer
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "POST", "/cards", """{suit: "hearts", number: 1}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		env.process( "GET", "/cards", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "suit": "hearts",
					|      "number": 1
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		env.process( "PUT", "/cards/1", """{number: 2, suit: "clubs"}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/cards", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "suit": "clubs",
					|      "number": 2
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

	"date" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource events
				|	title string
				|	event date
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "POST", "/events", """{title: "finish coding date support", event: "2017-06-14"}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		env.process( "GET", "/events", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "title": "finish coding date support",
					|      "event": "2017-06-14"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		env.process( "PUT", "/events/1", """{title: "finish coding date support", event: "2017-06-15"}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/events", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "title": "finish coding date support",
					|      "event": "2017-06-15"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

	"time" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource alarms
				|	title string
				|	alarm time
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "POST", "/alarms", """{title: "finish coding time support", alarm: "17:00:00"}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		env.process( "GET", "/alarms", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "title": "finish coding time support",
					|      "alarm": "17:00:00"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		env.process( "PUT", "/alarms/1", """{title: "finish coding time support", alarm: "16:45:00"}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/alarms", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "id": 1,
					|      "title": "finish coding time support",
					|      "alarm": "16:45:00"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

	"array" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource arrays
				|	a array(integer)
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "POST", "/arrays", """{a: []}""" ) shouldBe (SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin )
		env.process( "GET", "/arrays", null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "id": 1,
				|      "a": []
				|    }
				|  ]
				|}
			""".trim.stripMargin )
		env.process( "PUT", "/arrays/1", """{a: [1, 2, 3]}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/arrays", null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "id": 1,
				|      "a": [
				|        1,
				|        2,
				|        3
				|      ]
				|    }
				|  ]
				|}
			""".trim.stripMargin )
	}

	"blob" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource blobs
				|	a blob
				|	b blob (base64)
				|	c blob (hex)
				|	d blob (list)
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "POST", "/blobs", """{a: "AQID", b: "AQID", c: "010203", d: [1, 2, 3]}""" ) shouldBe (SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin )
		env.process( "GET", "/blobs", null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "id": 1,
				|      "a": "AQID",
				|      "b": "AQID",
				|      "c": "010203",
				|      "d": [
				|        1,
				|        2,
				|        3
				|      ]
				|    }
				|  ]
				|}
			""".trim.stripMargin )
//		env.process( "PUT", "/blobs/1", """{a: "AQIE", b: "AQIE", c: "010204", d: [1, 2, 4]}""" ) shouldBe (SC_NO_CONTENT, null, null)
//		env.process( "GET", "/blobs", null ) shouldBe (SC_OK, "application/json",
//			"""
//				|{
//				|  "data": [
//				|    {
//				|      "id": 1,
//				|      "a": "AQIE",
//				|      "b": "AQIE",
//				|      "c": "010204",
//				|      "d": [
//				|        1,
//				|        2,
//				|        4
//				|      ]
//				|    }
//				|  ]
//				|}
//			""".trim.stripMargin )
	}

	"binary" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource test
				|	a binary
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "POST", "/test", """{a: ""}""" ) shouldBe (SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin )
		env.process( "GET", "/test", null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "id": 1,
				|      "a": ""
				|    }
				|  ]
				|}
			""".trim.stripMargin )
		env.process( "PUT", "/test/1", """{a: "123457"}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/test", null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "id": 1,
				|      "a": "123457"
				|    }
				|  ]
				|}
			""".trim.stripMargin )
	}

}

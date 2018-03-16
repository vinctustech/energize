//@
package xyz.hyperreal.energize

import org.scalatest._
import prop.PropertyChecks

import org.apache.http.HttpStatus._


class DataTypeTests extends FreeSpec with PropertyChecks with Matchers {

	"boolean" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource truths
				|  statement   string  required
				|  veracity    boolean required
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/truths", null, """{statement: "crackers love cheese", veracity: true}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				 """.trim.stripMargin )
		pro.process( "POST", "/truths", null, """{statement: "we only have five senses", veracity: false}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/truths", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "statement": "crackers love cheese",
					|      "veracity": true
					|    },
					|    {
					|      "_id": 2,
					|      "statement": "we only have five senses",
					|      "veracity": false
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/truths/2", null, """{statement: "we only have 5 senses", veracity: false}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/truths", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "statement": "crackers love cheese",
					|      "veracity": true
					|    },
					|    {
					|      "_id": 2,
					|      "statement": "we only have 5 senses",
					|      "veracity": false
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

//	"enum" in {
//		val (c, s, d) = Test.dbconnect
//		val key = AUTHORIZATION.getString( "key" )
//		val config =
//			"""
//				|enum suit [clubs, diamonds, hearts, spades]
//				|
//				|resource cards
//				|	suit suit
//				|	number integer
//			""".trim.stripMargin
//		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )
//
//		pro.process( "POST", "/cards", null, """{suit: "hearts", number: 1}""", null ) shouldBe
//			(SC_CREATED, "application/json",
//				"""
//					|{
//					|  "data": 1
//					|}
//				""".trim.stripMargin )
//		pro.process( "GET", "/cards", null, null, null ) shouldBe
//			(SC_OK, "application/json",
//				"""
//					|{
//					|  "data": [
//					|    {
//					|      "_id": 1,
//					|      "suit": "hearts",
//					|      "number": 1
//					|    }
//					|  ]
//					|}
//				""".trim.stripMargin )
//		pro.process( "PUT", "/cards/1", null, """{number: 2, suit: "clubs"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
//		pro.process( "GET", "/cards", null, null, null ) shouldBe
//			(SC_OK, "application/json",
//				"""
//					|{
//					|  "data": [
//					|    {
//					|      "_id": 1,
//					|      "suit": "clubs",
//					|      "number": 2
//					|    }
//					|  ]
//					|}
//				""".trim.stripMargin )
//	}

	"text" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource documents
				|	document text
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/documents", null, """{document: "this is a document"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/documents", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "document": "this is a document"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
//		pro.process( "PUT", "/documents/1", null, """{document: "this is a longer document"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
//		pro.process( "GET", "/documents", null, null, null ) shouldBe
//			(SC_OK, "application/json",
//				"""
//					|{
//					|  "data": [
//					|    {
//					|      "_id": 1,
//					|      "document": "this is a longer document"
//					|    }
//					|  ]
//					|}
//				""".trim.stripMargin )

		c.close
	}

	"string" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource documents
				|	document string
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/documents", null, """{document: "this is a document"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/documents", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "document": "this is a document"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
				pro.process( "PUT", "/documents/1", null, """{document: "this is a longer document"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
				pro.process( "GET", "/documents", null, null, null ) shouldBe
					(SC_OK, "application/json",
						"""
							|{
							|  "data": [
							|    {
							|      "_id": 1,
							|      "document": "this is a longer document"
							|    }
							|  ]
							|}
						""".trim.stripMargin )

		c.close
	}

	"uuid" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource test
				|	identifier uuid
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/test", null, """{identifier: "a94cae4a-8bdf-49f3-849b-e7d338f4400a"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/test", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "identifier": "a94cae4a-8bdf-49f3-849b-e7d338f4400a"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/test/1", null, """{identifier: "8140d714-7387-486c-ab5b-ef5fc1cc790e"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/test", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "identifier": "8140d714-7387-486c-ab5b-ef5fc1cc790e"
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"float" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource r
				|	f float
				|
				|resource r1
				|	f float
				|
				|for a <- [1.3, .3, 1.3e3, .3e3, 1e3]
				| r1.insert( {f: a} )
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString(config), c, s, d, key )

		pro.process( "POST", "/r", null, """{f: 1.5}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "f": 1.5
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/r/1", null, """{f: 2.7}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/r", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "f": 2.7
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r1", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "f": 1.3
					|    },
					|    {
					|      "_id": 2,
					|      "f": 0.3
					|    },
					|    {
					|      "_id": 3,
					|      "f": 1300.0
					|    },
					|    {
					|      "_id": 4,
					|      "f": 300.0
					|    },
					|    {
					|      "_id": 5,
					|      "f": 1000.0
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"date" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource events
				|	title string
				|	event date
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString(config), c, s, d, key )

		pro.process( "POST", "/events", null, """{title: "finish coding date support", event: "2017-06-14"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/events", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "title": "finish coding date support",
					|      "event": "2017-06-14"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/events/1", null, """{title: "finish coding date support", event: "2017-06-15"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/events", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "title": "finish coding date support",
					|      "event": "2017-06-15"
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"time" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource alarms
				|	title string
				|	alarm time
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/alarms", null, """{title: "finish coding time support", alarm: "17:00:00"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/alarms", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "title": "finish coding time support",
					|      "alarm": "17:00:00"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/alarms/1", null, """{title: "finish coding time support", alarm: "16:45:00"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/alarms", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "title": "finish coding time support",
					|      "alarm": "16:45:00"
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"timestamp" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource lunar
				|	type string
				|	eclipse timestamp
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/lunar", null, """{type: "penumbral", eclipse: "2016-03-23T11:48:21.3Z"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/lunar", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "type": "penumbral",
					|      "eclipse": "2016-03-23T11:48:21.300Z"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/lunar/1", null, """{type: "penumbral", eclipse: "2017-03-23T11:47:11.8Z"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/lunar", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "type": "penumbral",
					|      "eclipse": "2017-03-23T11:47:11.800Z"
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"datetime" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource lunar
				|	type string
				|	eclipse datetime
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/lunar", null, """{type: "penumbral", eclipse: "2016-03-23T11:48:21.3Z"}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/lunar", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "type": "penumbral",
					|      "eclipse": "2016-03-23T11:48:21.300Z"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/lunar/1", null, """{type: "penumbral", eclipse: "2017-03-23T11:47:11.8Z"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/lunar", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "type": "penumbral",
					|      "eclipse": "2017-03-23T11:47:11.800Z"
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"decimal" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource r
				| a decimal(10, 2)
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/r", null, """{a: 1.01}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "a": 1.01
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		pro.process( "PUT", "/r/1", null, """{a: 0.5}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/r", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "a": 0.50
					|    }
					|  ]
					|}
				""".trim.stripMargin )

		c.close
	}

	"array" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource arrays
				|	a [integer]
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString(config), c, s, d, key )

		pro.process( "POST", "/arrays", null, """{a: []}""", null ) shouldBe (SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin )
		pro.process( "GET", "/arrays", null, null, null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
				|      "a": []
				|    }
				|  ]
				|}
			""".trim.stripMargin )
		pro.process( "PUT", "/arrays/1", null, """{a: [1, 2, 3]}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/arrays", null, null, null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
				|      "a": [
				|        1,
				|        2,
				|        3
				|      ]
				|    }
				|  ]
				|}
			""".trim.stripMargin )

		c.close
	}

	"blob" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource blobs
				|	a blob
				|	b blob (base64)
				|	c blob (hex)
				|	d blob (list)
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString(config), c, s, d, key )

		pro.process( "POST", "/blobs", null, """{a: "AQID", b: "AQID", c: "010203", d: [1, 2, 3]}""", null ) shouldBe (SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin )
		pro.process( "GET", "/blobs", null, null, null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
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
//		pro.process( "PUT", "/blobs/1", null, """{a: "AQIE", b: "AQIE", c: "010204", d: [1, 2, 4]}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
//		pro.process( "GET", "/blobs", null, null, null ) shouldBe (SC_OK, "application/json",
//			"""
//				|{
//				|  "data": [
//				|    {
//				|      "_id": 1,
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

		c.close
	}

	"binary" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource test
				|	a binary
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString(config), c, s, d, key )

		pro.process( "POST", "/test", null, """{a: ""}""", null ) shouldBe (SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin )
		pro.process( "GET", "/test", null, null, null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
				|      "a": ""
				|    }
				|  ]
				|}
			""".trim.stripMargin )
		pro.process( "PUT", "/test/1", null, """{a: "123457"}""", null ) shouldBe (SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/test", null, null, null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
				|      "a": "123457"
				|    }
				|  ]
				|}
			""".trim.stripMargin )

		c.close
	}

	"media" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource test
				|	a media
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/test", null, """{"a": "data:image/gif;base64,R0lGODlhAQABAIABAP///wAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="}""", null ) shouldBe(SC_CREATED, "application/json",
			"""
				|{
				|  "data": 1
				|}
			""".trim.stripMargin)
		pro.process( "GET", "/test", null, null, null ) shouldBe (SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
				|      "a": "/media/1"
				|    }
				|  ]
				|}
			""".trim.stripMargin)
		(pro.process( "GET", "/media/1", null, null, null ) match {case (sc, typ, data) => (sc, typ, data.asInstanceOf[Array[Byte]].toList)}) shouldBe
			(SC_OK, "image/gif", base642bytes("R0lGODlhAQABAIABAP///wAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==").toList)
		pro.process( "PUT", "/test/1", null, """{a: "data:,Hello, World!"}""", null ) shouldBe(SC_NO_CONTENT, "text/html", "No Content")
		pro.process( "GET", "/test", null, null, null ) shouldBe(SC_OK, "application/json",
			"""
				|{
				|  "data": [
				|    {
				|      "_id": 1,
				|      "a": "/media/2"
				|    }
				|  ]
				|}
			""".trim.stripMargin)
		(pro.process( "GET", "/media/2", null, null, null ) match {case (sc, typ, data) => (sc, typ, data.asInstanceOf[Array[Byte]].toList)}) shouldBe
			(SC_OK, "text/plain", "Hello, World!" map (_.toInt) toList)

		c.close
	}

}

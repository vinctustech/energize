package xyz.hyperreal.energize2

import org.scalatest._
import prop.PropertyChecks

import org.apache.http.HttpStatus._


class JSONConfigTests extends FreeSpec with PropertyChecks with Matchers {

	"basic" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|{
				|  "tables": [
				|    {
				|      "name": "r",
				|      "resource": true,
				|      "fields": [
				|        {
				|          "name": "limit",
				|          "type": {
				|            "category": "primitive",
				|            "type": "integer"
				|          }
				|        },
				|        {
				|          "name": "asdf",
				|          "type": {
				|            "category": "primitive",
				|            "type": "boolean"
				|          }
				|        }
				|      ]
				|    }
				|  ]
				|}
			""".trim.stripMargin
		val (pro, _) = Definition.defineFromJSON( io.Source.fromString( config ), c, s, d, key )

		pro.process( "POST", "/r", null, """{"limit": 123, "asdf": true}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					 |{
					 |  "data": 1
					 |}
				 """.trim.stripMargin )
		pro.process( "GET", "/r", null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "limit": 123,
					|      "asdf": true
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

//	"simple js" in {
//		val (c, s, d) = Test.dbconnect
//		val key = AUTHORIZATION.getString( "key" )
//		val config =
//			"""
//				|{
//				|  "routes": [
//				|    {
//				|      "mappings": [
//				|        {
//				|          "method": "GET",
//				|          "path": "/js",
//				|          "language": "ECMAScript",
//				|          "action": "Ok( 1 + 2 )"
//				|        }
//				|      ]
//				|    }
//				|  ]
//				|}
//			""".trim.stripMargin
//		val (pro, _) = Definition.defineFromJSON( io.Source.fromString( config ), c, s, d, key )
//
//		pro.process( "GET", "/js", null, null ) shouldBe
//			(SC_OK, "application/json",
//				"""
//					|{
//					|  "data": 3
//					|}
//				""".trim.stripMargin )
//	}

}

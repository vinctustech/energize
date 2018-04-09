package xyz.hyperreal.energize

import org.apache.http.HttpStatus.{SC_CREATED, SC_NO_CONTENT, SC_OK}
import org.scalatest._
import prop.PropertyChecks


class AuthorizationTests extends FreeSpec with PropertyChecks with Matchers {

//	"protected resource" in {
//		val (c, s, d) = Test.dbconnect
//		val key = AUTHENTICATION.getString( "key" )
//		val config =
//			"""
//				|resource r protected
//				|  f string
//			""".trim.stripMargin
//		val pro = Definition.define( io.Source.fromString( config ), c, s, d, key )
//
//		pro.process( "GET", "/r", new SimpleMessage("Authorization" -> """Bearer """), null, null ) shouldBe
//			(SC_OK, "application/json",
//				"""
//					|{
//					|  "data": [
//					|    {
//					|      "_id": 1,
//					|      "f": "blah",
//					|    },
//					|  ]
//					|}
//				""".trim.stripMargin )
//
//		c.close
//	}

}
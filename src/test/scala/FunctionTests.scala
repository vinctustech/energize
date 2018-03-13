package xyz.hyperreal.energize2

import org.scalatest._
import prop.PropertyChecks

import org.apache.http.HttpStatus.{SC_CREATED, SC_OK, SC_NOT_FOUND}


class FunctionTests extends FreeSpec with PropertyChecks with Matchers {

	"aggregates" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHENTICATION.getString( "key" )
		val config =
			"""
				|resource r
				|	i	integer
				|	f	float
			""".trim.stripMargin
		val pro = Definition.define( io.Source.fromString(config), c, s, d, key )

		pro.process( "GET", "/r/count", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/sum/i", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/sum/f", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0.0
					|}
				""".trim.stripMargin )
		pro.process( "POST", "/r", null, """{"i": 3, "f": 3}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					 |{
					 |  "data": 1
					 |}
				""".trim.stripMargin )
		pro.process( "POST", "/r", null, """{"i": 4, "f": 4}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		pro.process( "POST", "/r", null, """{"i": 5, "f": 5}""", null ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 3
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/count", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 3
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/sum/i", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 12
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/sum/f", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 12.0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/max/i", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/max/f", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5.0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/avg/n", null, null, null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "field 'n' not found"
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/avg/i", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/avg/f", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4.0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/min/n", null, null, null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "field 'n' not found"
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/min/i", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 3
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/min/f", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 3.0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/count?filter=i%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/sum/i?filter=i%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 9
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/max/i?filter=i%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/avg/i?filter=i%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/min/i?filter=i%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/sum/f?filter=f%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 9.0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/max/f?filter=f%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5.0
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/avg/f?filter=f%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4.5
					|}
				""".trim.stripMargin )
		pro.process( "GET", "/r/min/f?filter=f%3E3", null, null, null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4.0
					|}
				""".trim.stripMargin )
	}

}

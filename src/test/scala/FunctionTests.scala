package xyz.hyperreal.energize

import org.apache.http.HttpStatus.{SC_CREATED, SC_OK, SC_NOT_FOUND}
import org.scalatest._
import prop.PropertyChecks


class FunctionTests extends FreeSpec with PropertyChecks with Matchers {

	"aggregates" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource r
				|	i	integer
				|	f	float
				|			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "GET", "/r/count", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/sum/i", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/sum/f", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0.0
					|}
				""".trim.stripMargin )
		env.process( "POST", "/r", """{"i": 3, "f": 3}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					 |{
					 |  "data": 1
					 |}
				 """.trim.stripMargin )
		env.process( "POST", "/r", """{"i": 4, "f": 4}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		env.process( "POST", "/r", """{"i": 5, "f": 5}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 3
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/count", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 3
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/sum/i", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 12
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/sum/f", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 12.0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/max/i", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/max/f", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5.0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/avg/n", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "field 'n' not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/avg/i", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/avg/f", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4.0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/min/n", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "field 'n' not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/min/i", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 3
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/min/f", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 3.0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/count?filter=i%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/sum/i?filter=i%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 9
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/max/i?filter=i%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/avg/i?filter=i%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/min/i?filter=i%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/sum/f?filter=f%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 9.0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/max/f?filter=f%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 5.0
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/avg/f?filter=f%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4.5
					|}
				""".trim.stripMargin )
		env.process( "GET", "/r/min/f?filter=f%3E3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 4.0
					|}
				""".trim.stripMargin )
	}

}
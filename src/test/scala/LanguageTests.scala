package xyz.hyperreal.energize

import org.scalatest._
import prop.PropertyChecks


class LanguageTests extends FreeSpec with PropertyChecks with Matchers {
	
	"assignment" in {
		val code =
			"""
			|var a = 123
			|
			|a = a + 456/2
			|print( a )
			""".trim.stripMargin
		Test.capture( code ) shouldBe "351"
	}
	
	"while" in {
		val code =
			"""
			|var a = 3
			|var b = 0
			|
			|while a > 0
			|  b = b + a
			|  a = a - 1
			|
			|print( b )
			""".trim.stripMargin
		Test.capture( code ) shouldBe "6"
	}
	
}
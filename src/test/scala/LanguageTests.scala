package xyz.hyperreal.cras

import org.scalatest._
import prop.PropertyChecks


class LanguageTests extends FreeSpec with PropertyChecks with Matchers {
	
	"while" in {
		val code =
			"""
			|var a = 123
			|
			|
			|print( a )
			|
			""".trim.stripMargin
		Test.capture( code ) shouldBe "123"
	}
	
}
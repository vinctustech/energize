package xyz.hyperreal.energize2

import org.scalatest._
import prop.PropertyChecks


class EnergizeTests extends FreeSpec with PropertyChecks with Matchers {

	"routes" in {
		runCapture(
			"""
				|val a = 2
				|
				|routes
				|  GET / if a == 2 => 'two'
				|  GET / => (a, req)
				|  GET /p: (a|b|c)/asdf => req
				|  GET /blah/from:*-"to": => (req.path.from, req.path."to")
				|  GET /opt/p:(ab?cd) => req
				|  GET /rep1/p:(ab+cd) => req
				|  GET /rep/p:(ab{3}cd) => req
				|  GET /repp/p:((ab){3}cd) => req
				|  GET /asdf/p:*/zxvc => req
				|
				|write( _router_( 'GET', '/', null, null, null ) )
				|write( _router_( 'GET', '/b/asdf', null, null, null ) )
				|write( _router_( 'GET', '/blah/123-456', null, null, null ) )
				|write( _router_( 'GET', '/opt/abcd', null, null, null ) )
				|write( _router_( 'GET', '/opt/acd', null, null, null ) )
				|write( _router_( 'GET', '/rep1/abcd', null, null, null ) )
				|write( _router_( 'GET', '/rep1/abbcd', null, null, null ) )
				|write( _router_( 'GET', '/rep1/acd', null, null, null ) )
				|write( _router_( 'GET', '/rep/abbbcd', null, null, null ) )
				|write( _router_( 'GET', '/rep/abbcd', null, null, null ) )
				|write( _router_( 'GET', '/repp/abababcd', null, null, null ) )
				|write( _router_( 'GET', '/repp/ababcd', null, null, null ) )
				|write( _router_( 'GET', '/asdf/wow/zxvc', null, null, null ) )
			""".stripMargin
		) shouldBe
			"""
				|two
				|{"method": "GET", "body": null, "path": {"p": "b"}, "query": null, "parms": null}
				|(123, 456)
				|{"method": "GET", "body": null, "path": {"p": "abcd"}, "query": null, "parms": null}
				|{"method": "GET", "body": null, "path": {"p": "acd"}, "query": null, "parms": null}
				|{"method": "GET", "body": null, "path": {"p": "abcd"}, "query": null, "parms": null}
				|{"method": "GET", "body": null, "path": {"p": "abbcd"}, "query": null, "parms": null}
				|no match
				|{"method": "GET", "body": null, "path": {"p": "abbbcd"}, "query": null, "parms": null}
				|no match
				|{"method": "GET", "body": null, "path": {"p": "abababcd"}, "query": null, "parms": null}
				|no match
				|{"method": "GET", "body": null, "path": {"p": "wow"}, "query": null, "parms": null}
				|
			""".stripMargin.trim
	}

}
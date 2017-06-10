package xyz.hyperreal.energize

import org.scalatest._
import prop.PropertyChecks

import org.apache.http.HttpStatus._


class ProcessTests extends FreeSpec with PropertyChecks with Matchers {
	
	"empty database" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
			|resource todo /api/v1
			|  name        string  required
			|  description string  optional
			|  status      integer required
			|
			|resource test /api/v1
			|  asdf        integer required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "GET", "/api/v1/todo", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": []
					|}
				""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo/size", null ) shouldBe
			(SC_OK, "application/json", """
								|{
								|  "data": 0
								|}
							""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/size", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 0
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/api/v1/tod", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""{
					|  "error": "route not found"
					|}""".stripMargin)
		c.close
	}
	
	"empty database (no base)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
			|resource todo
			|  name        string  required
			|  description string  optional
			|  status      integer required
			|
			|resource test
			|  asdf        integer required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "GET", "/todo", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/test", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/todo/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/test/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/tod", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""{
					|  "error": "route not found"
					|}""".stripMargin)
		c.close
	}
	
	"post/put/get/delete" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
			|resource todo /api/v1
			|	name        string  required
			|	description string  optional
			|	status      integer required
			|
			|resource test /api/v1
			|  asdf       integer required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "POST", "/api/v1/todo", """{"name": "do something", "status": 1}""" ) shouldBe
			(SC_CREATED, "application/json", """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "POST", "/api/v1/test", """{"asdf": 123}""" ) shouldBe
			(SC_CREATED, "application/json", """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "name": "do something",
			|      "description": null,
			|      "status": 1
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "id": 1,
			|    "name": "do something",
			|    "description": null,
			|    "status": 1
			|  }
			|}
			""".trim.stripMargin )
		env.process( "PUT", "/api/v1/todo/1", """{"name": "do something else", "status": 1}""" ) shouldBe
			(SC_BAD_REQUEST, "application/json",
				"""
					|{
					|  "error": "update: missing field(s): description"
					|}
				""".trim.stripMargin)
		env.process( "PUT", "/api/v1/todo/1", """{"name": "do something else", "status": 1, description: null, extra: null}""" ) shouldBe
			(SC_BAD_REQUEST, "application/json",
				"""
					|{
					|  "error": "update: excess field(s): extra"
					|}
				""".trim.stripMargin)
		env.process( "PUT", "/api/v1/todo/1", """{"name": "do something else", "status": 1, description: null}""" ) shouldBe
			(SC_NO_CONTENT, null, null)
		env.process( "PUT", "/api/v1/test/1", """{"asdf": 1234}""" ) shouldBe
			(SC_NO_CONTENT, null, null)
		env.process( "GET", "/api/v1/todo", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": [
																		|    {
																		|      "id": 1,
																		|      "name": "do something else",
																		|      "description": null,
																		|      "status": 1
																		|    }
																		|  ]
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": {
																		|    "id": 1,
																		|    "name": "do something else",
																		|    "description": null,
																		|    "status": 1
																		|  }
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo/size", null ) shouldBe
			(SC_OK, "application/json", """
								|{
								|  "data": 1
								|}
							""".trim.stripMargin )
		env.process( "DELETE", "/api/v1/todo/1", null ) shouldBe (SC_NO_CONTENT, null, null)
 		env.process( "GET", "/api/v1/todo/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/api/v1/test", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "asdf": 1234
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "id": 1,
			|    "asdf": 1234
			|  }
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/size", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		env.process( "DELETE", "/api/v1/test/1", null ) shouldBe (SC_NO_CONTENT, null, null)
 		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		c.close
	}
	
	"functions/evaluation" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
			|def f( x, y ) = {"a": x, "b": y, "sum": x + y}
			|
			|routes
			|	GET   /f/a:integer/b:integer Ok( f(/a, /b) )
			|	GET   /plus/a:/b:            Ok( /a + /b )
			|	GET   /combine               Ok( {"a": 1} + $entity )
			|	GET   /eval                  Ok( toString(eval($entity.expr)) )			# GET /eval {"expr": "3 + 4"}
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "GET", "/eval", """ {"expr": "(i + 2)/2*i"} """ ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": "-1/2+i"
			|}
			""".trim.stripMargin )
		env.process( "GET", "/f/3/4", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "a": 3,
			|    "b": 4,
			|    "sum": 7
			|  }
			|}
			""".trim.stripMargin )
		c.close
	}
	
	"empty database (one-to-many)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
			|resource products
			|  code       string  unique required
			|  type       types   required
			|
			|resource types
			|  name       string  unique required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "GET", "/products", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
		env.process( "GET", "/types", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": []
			|}
			""".trim.stripMargin )
 		env.process( "GET", "/products/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/types/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
 		env.process( "GET", "/product", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""{
					|  "error": "route not found"
					|}""".stripMargin )
		c.close
	}
	
	"post/get/put/delete (one-to-many)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource products
				|  code       string  unique required
				|  type       types   required
				|
				|resource types
				|  name       string  unique required
				|			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "POST", "/types", """{"name": "normal"}""" ) shouldBe
			(SC_CREATED, "application/json", """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "POST", "/types", """{"name": "normal"}""" ) shouldBe
			(SC_CONFLICT, "application/json", """
																				 |{
																				 |  "error": "Unique index or primary key violation: \"CONSTRAINT_INDEX_4C ON PUBLIC.TYPES(NAME) VALUES ('normal', 1)\"; SQL statement:\nINSERT INTO types (name) VALUES (?) [23505-194]"
																				 |}
																			 """.trim.stripMargin )
		env.process( "POST", "/types", """{"name": "special"}""" ) shouldBe
			(SC_CREATED, "application/json", """
																				 |{
																				 |  "data": 3
																				 |}
																			 """.trim.stripMargin )
		env.process( "POST", "/products", """{"code": "12345", "type": "normal"}""" ) shouldBe
			(SC_CREATED, "application/json",  """
			|{
			|  "data": 1
			|}
			""".trim.stripMargin )
		env.process( "GET", "/products", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "code": "12345",
			|      "type": {
			|        "id": 1,
			|        "name": "normal"
			|      }
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/products/1", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "id": 1,
			|    "code": "12345",
			|    "type": {
			|      "id": 1,
			|      "name": "normal"
			|    }
			|  }
			|}
			""".trim.stripMargin )
		env.process( "PUT", "/products/1", """{"code": "123456", "type": "special"}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/products", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": [
																		|    {
																		|      "id": 1,
																		|      "code": "123456",
																		|      "type": {
																		|        "id": 3,
																		|        "name": "special"
																		|      }
																		|    }
																		|  ]
																		|}
																	""".trim.stripMargin )
		env.process( "DELETE", "/products/1", null ) shouldBe (SC_NO_CONTENT, null, null)
 		env.process( "GET", "/products/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/types", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": [
			|    {
			|      "id": 1,
			|      "name": "normal"
			|    },
			|    {
			|      "id": 3,
			|      "name": "special"
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/types/1", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "id": 1,
			|    "name": "normal"
			|  }
			|}
			""".trim.stripMargin )
		env.process( "DELETE", "/types/1", null ) shouldBe (SC_NO_CONTENT, null, null)
 		env.process( "GET", "/types/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		c.close
	}

	"empty database (many-to-many)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource customers
				|  lastname   string  required
				|  firstname  string  required
				|  products   products array
				|
				|resource products
				|  name       string  unique required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "GET", "/products", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": []
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/customers", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": []
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/products/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/customers/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/product", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""{
					|  "error": "route not found"
					|}""".stripMargin )
		c.close
	}

	"post/get/put/delete (many-to-many)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource customers
				|  lastname   string  required
				|  firstname  string  required
				|  products   products array
				|
				|resource products
				|  name       string  unique required
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString(config), c, s, d, key )

		env.process( "POST", "/products", """{"name": "savings"}""" ) shouldBe
			(SC_CREATED, "application/json", """
																				 |{
																				 |  "data": 1
																				 |}
																			 """.trim.stripMargin )
		env.process( "POST", "/products", """{"name": "savings"}""" ) shouldBe
			(SC_CONFLICT, "application/json", """
																					|{
																					|  "error": "Unique index or primary key violation: \"CONSTRAINT_INDEX_F ON PUBLIC.PRODUCTS(NAME) VALUES ('savings', 1)\"; SQL statement:\nINSERT INTO products (name) VALUES (?) [23505-194]"
																					|}
																				""".trim.stripMargin )
		env.process( "POST", "/products", """{"name": "credit card"}""" ) shouldBe
			(SC_CREATED, "application/json", """
																				 |{
																				 |  "data": 3
																				 |}
																			 """.trim.stripMargin )
		env.process( "POST", "/customers", """{"lastname": "doe", "firstname": "john"}""" ) shouldBe
			(SC_CREATED, "application/json",  """
																					|{
																					|  "data": 1
																					|}
																				""".trim.stripMargin )
		env.process( "GET", "/customers", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": [
																		|    {
																		|      "id": 1,
																		|      "lastname": "doe",
																		|      "firstname": "john",
																		|      "products": []
																		|    }
																		|  ]
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/customers/1", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": {
																		|    "id": 1,
																		|    "lastname": "doe",
																		|    "firstname": "john",
																		|    "products": []
																		|  }
																		|}
																	""".trim.stripMargin )
		env.process( "POST", "/customers/1/products/1", null ) shouldBe (SC_NO_CONTENT, null, null)
//		env.process( "PUT", "/products/1", """{"code": "123456", "type": "special"}""" ) shouldBe (SC_NO_CONTENT, null, null)
//		env.process( "GET", "/products", null ) shouldBe
//			(SC_OK, "application/json", """
//																		|{
//																		|  "data": [
//																		|    {
//																		|      "id": 1,
//																		|      "code": "123456",
//																		|      "type": {
//																		|        "id": 3,
//																		|        "name": "special"
//																		|      }
//																		|    }
//																		|  ]
//																		|}
//																	""".trim.stripMargin )
//		env.process( "DELETE", "/products/1", null ) shouldBe (SC_NO_CONTENT, null, null)
//		env.process( "GET", "/products/1", null ) shouldBe
//			(SC_NOT_FOUND, "application/json",
//				"""
//					|{
//					|  "error": "id 1 not found"
//					|}
//				""".trim.stripMargin )
//		env.process( "GET", "/types", null ) shouldBe
//			(SC_OK, "application/json", """
//																		|{
//																		|  "data": [
//																		|    {
//																		|      "id": 1,
//																		|      "name": "normal"
//																		|    },
//																		|    {
//																		|      "id": 3,
//																		|      "name": "special"
//																		|    }
//																		|  ]
//																		|}
//																	""".trim.stripMargin )
//		env.process( "GET", "/types/1", null ) shouldBe
//			(SC_OK, "application/json", """
//																		|{
//																		|  "data": {
//																		|    "id": 1,
//																		|    "name": "normal"
//																		|  }
//																		|}
//																	""".trim.stripMargin )
//		env.process( "DELETE", "/types/1", null ) shouldBe (SC_NO_CONTENT, null, null)
//		env.process( "GET", "/types/1", null ) shouldBe
//			(SC_NOT_FOUND, "application/json",
//				"""
//					|{
//					|  "error": "id 1 not found"
//					|}
//				""".trim.stripMargin )
		c.close
	}
}
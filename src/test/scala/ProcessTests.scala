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
		env.process( "GET", "/api/v1/todo/count", null ) shouldBe
			(SC_OK, "application/json", """
								|{
								|  "data": 0
								|}
							""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/count", null ) shouldBe
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
			|      "_id": 1,
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
			|    "_id": 1,
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
																		|      "_id": 1,
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
																		|    "_id": 1,
																		|    "name": "do something else",
																		|    "description": null,
																		|    "status": 1
																		|  }
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/api/v1/todo/count", null ) shouldBe
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
			|      "_id": 1,
			|      "asdf": 1234
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/1", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "_id": 1,
			|    "asdf": 1234
			|  }
			|}
			""".trim.stripMargin )
		env.process( "GET", "/api/v1/test/count", null ) shouldBe
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
			(SC_CONFLICT, "application/json", s"""
																				 |{
																				 |  "error": "Unique index or primary key violation: \\"CONSTRAINT_INDEX_4C ON PUBLIC.TYPES(${nameIn("NAME")}) VALUES ('normal', 1)\\"; SQL statement:\\nINSERT INTO types (${nameIn("name")}) VALUES (?) [23505-196]"
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
			|      "_id": 1,
			|      "code": "12345",
			|      "type": {
			|        "_id": 1,
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
			|    "_id": 1,
			|    "code": "12345",
			|    "type": {
			|      "_id": 1,
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
																		|      "_id": 1,
																		|      "code": "123456",
																		|      "type": {
																		|        "_id": 3,
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
			|      "_id": 1,
			|      "name": "normal"
			|    },
			|    {
			|      "_id": 3,
			|      "name": "special"
			|    }
			|  ]
			|}
			""".trim.stripMargin )
		env.process( "GET", "/types/1", null ) shouldBe
			(SC_OK, "application/json", """
			|{
			|  "data": {
			|    "_id": 1,
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
				|  products   [products]
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
				|  products   [products]
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
			(SC_CONFLICT, "application/json", s"""
																					|{
																					|  "error": "Unique index or primary key violation: \\"CONSTRAINT_INDEX_F ON PUBLIC.PRODUCTS(${nameIn("NAME")}) VALUES ('savings', 1)\\"; SQL statement:\\nINSERT INTO products (${nameIn("name")}) VALUES (?) [23505-196]"
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
																		|      "_id": 1,
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
																		|    "_id": 1,
																		|    "lastname": "doe",
																		|    "firstname": "john",
																		|    "products": []
																		|  }
																		|}
																	""".trim.stripMargin )
		env.process( "POST", "/customers/1/products/target/1", null ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "PUT", "/customers/1", """{"lastname": "doe", "firstname": "jane"}""" ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/customers", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": [
																		|    {
																		|      "_id": 1,
																		|      "lastname": "doe",
																		|      "firstname": "jane",
																		|      "products": [
																		|        {
																		|          "_id": 1,
																		|          "name": "savings"
																		|        }
																		|      ]
																		|    }
																		|  ]
																		|}
																	""".trim.stripMargin )
		env.process( "DELETE", "/customers/1", null ) shouldBe (SC_NO_CONTENT, null, null)
		env.process( "GET", "/customers/1", null ) shouldBe
			(SC_NOT_FOUND, "application/json",
				"""
					|{
					|  "error": "id 1 not found"
					|}
				""".trim.stripMargin )
		env.process( "GET", "/products", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": [
																		|    {
																		|      "_id": 1,
																		|      "name": "savings"
																		|    },
																		|    {
																		|      "_id": 3,
																		|      "name": "credit card"
																		|    }
																		|  ]
																		|}
																	""".trim.stripMargin )
		env.process( "GET", "/products/1", null ) shouldBe
			(SC_OK, "application/json", """
																		|{
																		|  "data": {
																		|    "_id": 1,
																		|    "name": "savings"
																		|  }
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
		c.close
	}

	"post/get/put/delete (mixed relationships)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource books
				|	title string
				|	authors [authors]
				|	publisher publishers
				|
				|resource authors
				|	name string unique
				|
				|resource publishers
				|	name string unique
				|
				|
				|if count( books, None ) == 0
				|	insert( publishers, {name: "Spectra"} )
				|	insert( books, {title: "Dune: House Atreides", publisher: "Spectra"} )	# could also write `publisher: 1`		""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "POST","/books/1/authors", """{"name": "Brian Herbert"}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 1
					|}
				""".trim.stripMargin )
		env.process( "POST", "/books/1/authors", """{"name": "Kevin J. Anderson"}""" ) shouldBe
			(SC_CREATED, "application/json",
				"""
					|{
					|  "data": 2
					|}
				""".trim.stripMargin )
		env.process( "GET", "/books", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "title": "Dune: House Atreides",
					|      "authors": [
					|        {
					|          "_id": 1,
					|          "name": "Brian Herbert"
					|        },
					|        {
					|          "_id": 2,
					|          "name": "Kevin J. Anderson"
					|        }
					|      ],
					|      "publisher": {
					|        "_id": 1,
					|        "name": "Spectra"
					|      }
					|    }
					|  ]
					|}
				""".trim.stripMargin)
	}

	"request query parameters (no relationship) 1" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource data
				|	first_name string indexed
				|	last_name string indexed
				|	company_name string indexed
				|	address string indexed
				|	city string indexed
				|	province string indexed
				|	postal string indexed
				|	phone1 string indexed
				|	phone2 string indexed
				|	email string indexed
				|	web string indexed
				|
				|batchInsert( data, [
				|	["Francoise","Rautenstrauch","Riebesell, H F Jr","2335 Canton Hwy #6","Windsor","ON","N8N 3N2","519-569-8399","519-978-6179","francoise.rautenstrauch@rautenstrauch.com","http://www.riebesellhfjr.com"],
				|	["Kendra","Loud","Deloitte & Touche","6 Arch St #9757","Alcida","NB","E8J 2C4","506-363-1526","506-932-4472","kloud@gmail.com","http://www.deloittetouche.com"],
				|	["Lourdes","Bauswell","Oklahoma Neon Inc","9547 Belmont Rd #21","Belleville","ON","K8P 1B3","613-903-7043","613-638-6682","lourdes_bauswell@aol.com","http://www.oklahomaneoninc.com"],
				|	["Hannah","Edmison","M B A Paint Stores","73 Pittsford Victor Rd","Vancouver","BC","V5Z 3K2","604-334-3686","604-692-7694","hannah@yahoo.com","http://www.mbapaintstores.com"],
				|	["Tom","Loeza","Sheraton Shreveport Hotel","447 Commercial St Se","LIle-Perrot","QC","J7V 4T4","514-487-6096","514-727-4760","tom.loeza@gmail.com","http://www.sheratonshreveporthotel.com"],
				|	["Queenie","Kramarczyk","Goeman Wood Products Inc","47 Garfield Ave","Swift Current","SK","S9H 4V2","306-421-5793","306-302-7591","queenie.kramarczyk@kramarczyk.org","http://www.goemanwoodproductsinc.com"],
				|	["Hui","Portaro","A Storage Inn Of Gloucester","3 Mill Rd","Baker Brook","NB","E7A 1T3","506-827-7755","506-276-4830","hui_portaro@cox.net","http://www.astorageinnofgloucester.com"],
				|	["Josefa","Opitz","Norman Gale Isuzu","136 W Grand Ave #3","Delhi","ON","N4B 1C4","519-788-7645","519-526-3721","josefa.opitz@opitz.org","http://www.normangaleisuzu.com"],
				|	["Lea","Steinhaus","James, Christopher Esq","80 Maplewood Dr #34","Bradford","ON","L3Z 2S4","905-618-8258","905-651-3298","lsteinhaus@cox.net","http://www.jameschristopheresq.com"],
				|	["Paola","Vielma","Congress Title","58 Hancock St","Aurora","ON","L4G 2J7","905-456-1117","905-263-7711","paola_vielma@aol.com","http://www.congresstitle.com"],
				|	["Hortencia","Bresser","Batavia Chamber Of Commerce","808 Calle De Industrias","New Waterford","NS","B1H 1Z4","902-256-6791","902-370-8282","hbresser@aol.com","http://www.bataviachamberofcommerce.com"],
				|	["Leanna","Tijerina","Stephenson Land Surveying","2859 Dorsett Rd","North York","ON","M9L 2T9","416-719-2114","416-658-1773","leanna@cox.net","http://www.stephensonlandsurveying.com"],
				|	["Danilo","Pride","Harry L Adams Incorporated","6857 Wall St","Red Deer","AB","T4R 2H5","403-212-4945","403-888-9985","danilo_pride@hotmail.com","http://www.harryladamsincorporated.com"],
				|	["Huey","Marcille","Southern Idaho Pipe & Stl Corp","169 Journal Sq","Edmonton","AB","T5P 1G9","780-639-3619","780-520-1241","huey.marcille@gmail.com","http://www.southernidahopipestlcorp.com"],
				|	["Apolonia","Warne","Kitchen People","3 E 31st St #77","Fredericton","NB","E3G 0A3","506-978-1488","506-221-1874","apolonia@gmail.com","http://www.kitchenpeople.com"],
				|	["Chandra","Lagos","Meredith Realty Group Inc","7 N Dean St","Etobicoke","ON","M8Z 3P6","416-716-6446","416-822-1760","chandra.lagos@lagos.org","http://www.meredithrealtygroupinc.com"],
				|	["Crissy","Pacholec","Cgi Systems Inc","85 S State St","Barrie","ON","L4N 6T7","705-477-2307","705-523-6746","crissy@aol.com","http://www.cgisystemsinc.com"],
				|	["Gianna","Branin","All Brevard Cert Apprsls Inc","100 Main St","Calgary","AB","T2K 4X5","403-337-7162","403-540-5944","gianna@aol.com","http://www.allbrevardcertapprslsinc.com"],
				|	["Valentin","Billa","General Color Co Inc","6185 Bohn St #72","Pangman","SK","S0C 2C0","306-291-5073","306-316-7477","vbilla@yahoo.com","http://www.generalcolorcoinc.com"],
				|	["Ilona","Dudash","Adams Balcom & Larose Pc","2 Sutton Pl S #5727","Rouyn-Noranda","QC","J9X 3V4","819-536-7034","819-413-1530","idudash@dudash.com","http://www.adamsbalcomlarosepc.com"]], false )
				""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config), c, s, d, key )

		env.process( "GET", "/data/20?fields=_id,first_name", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": {
					|    "_id": 20,
					|    "first_name": "Ilona"
					|  }
					|}
				""".trim.stripMargin )
		env.process( "GET", "/data/20?fields=first_name", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": {
					|    "first_name": "Ilona"
					|  }
					|}
				""".trim.stripMargin )
		env.process( "GET", "/data?fields=first_name,city;order=city:asc;filter=first_name~*%25C%25", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "first_name": "Crissy",
					|      "city": "Barrie"
					|    },
					|    {
					|      "first_name": "Chandra",
					|      "city": "Etobicoke"
					|    },
					|    {
					|      "first_name": "Hortencia",
					|      "city": "New Waterford"
					|    },
					|    {
					|      "first_name": "Francoise",
					|      "city": "Windsor"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		env.process( "GET", "/data?fields=_id,first_name;limit=3;page=2", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 4,
					|      "first_name": "Hannah"
					|    },
					|    {
					|      "_id": 5,
					|      "first_name": "Tom"
					|    },
					|    {
					|      "_id": 6,
					|      "first_name": "Queenie"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

	"request query parameters (no relationship) 2" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource customers
				|	CustomerName string indexed
				|	ContactName string indexed
				|	Address string
				|	City string indexed
				|	PostalCode string indexed
				|	Country string indexed
				|
				|if count( customers, None ) == 0
				|	batchInsert( customers, [
				|	 ["Alfreds Futterkiste",                "Maria Anders",       "Obere Str. 57",                 "Berlin",      "12209",    "Germany"],
				|	 ["Ana Trujillo Emparedados y helados", "Ana Trujillo",       "Avda. de la Constitución 2222", "México D.F.", "05021",    "Mexico"],
				|	 ["Antonio Moreno Taquería",            "Antonio Moreno",     "Mataderos 2312",                "México D.F.", "05023",    "Mexico"],
				|	 ["Around the Horn",                    "Thomas Hardy",       "120 Hanover Sq.",               "London",      "WA1 1DP",  "UK"],
				|	 ["Berglunds snabbköp",                 "Christina Berglund", "Berguvsvägen 8",                "Luleå",       "S-958 22", "Sweden"]], false )
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "GET", "/customers?order=City:asc,PostalCode:desc", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "CustomerName": "Alfreds Futterkiste",
					|      "ContactName": "Maria Anders",
					|      "Address": "Obere Str. 57",
					|      "City": "Berlin",
					|      "PostalCode": "12209",
					|      "Country": "Germany"
					|    },
					|    {
					|      "_id": 4,
					|      "CustomerName": "Around the Horn",
					|      "ContactName": "Thomas Hardy",
					|      "Address": "120 Hanover Sq.",
					|      "City": "London",
					|      "PostalCode": "WA1 1DP",
					|      "Country": "UK"
					|    },
					|    {
					|      "_id": 5,
					|      "CustomerName": "Berglunds snabbköp",
					|      "ContactName": "Christina Berglund",
					|      "Address": "Berguvsvägen 8",
					|      "City": "Luleå",
					|      "PostalCode": "S-958 22",
					|      "Country": "Sweden"
					|    },
					|    {
					|      "_id": 3,
					|      "CustomerName": "Antonio Moreno Taquería",
					|      "ContactName": "Antonio Moreno",
					|      "Address": "Mataderos 2312",
					|      "City": "México D.F.",
					|      "PostalCode": "05023",
					|      "Country": "Mexico"
					|    },
					|    {
					|      "_id": 2,
					|      "CustomerName": "Ana Trujillo Emparedados y helados",
					|      "ContactName": "Ana Trujillo",
					|      "Address": "Avda. de la Constitución 2222",
					|      "City": "México D.F.",
					|      "PostalCode": "05021",
					|      "Country": "Mexico"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
		env.process( "GET", "/customers?filter=CustomerName~A%25,City=Berlin", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "CustomerName": "Alfreds Futterkiste",
					|      "ContactName": "Maria Anders",
					|      "Address": "Obere Str. 57",
					|      "City": "Berlin",
					|      "PostalCode": "12209",
					|      "Country": "Germany"
					|    }
					|  ]
					|}
				""".trim.stripMargin )
	}

	"request query parameters (one-to-many)" in {
		val (c, s, d) = Test.dbconnect
		val key = AUTHORIZATION.getString( "key" )
		val config =
			"""
				|resource books
				|	title string required
				|	author authors
				|	publisher publishers
				|
				|resource authors
				|	name string unique required
				|
				|resource publishers
				|	name string unique required
				|
				|
				|if count( books, None ) == 0
				|	batchInsert( publishers, [["North Point Press"], ["Enhanced Media"], ["Amazon Classics"]], false )
				|
				|	insert( authors, {name: "Fyodor Dostoyevsky"} )
				|	insert( authors, {name: "Lewis Carroll"} )
				|	insert( authors, {name: "Mark Twain"} )
				|
				|	insert( books, {title: "The Brothers Karamazov", author: "Fyodor Dostoyevsky", publisher: "North Point Press"} )
				|	insert( books, {title: "Alice's Adventures in Wonderland", author: "Lewis Carroll", publisher: "Enhanced Media"} )
				|	insert( books, {title: "The Adventures of Huckleberry Finn", author: "Mark Twain", publisher: "Amazon Classics"} )
			""".trim.stripMargin
		val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

		env.process( "GET", "/books", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 1,
					|      "title": "The Brothers Karamazov",
					|      "author": {
					|        "_id": 1,
					|        "name": "Fyodor Dostoyevsky"
					|      },
					|      "publisher": {
					|        "_id": 1,
					|        "name": "North Point Press"
					|      }
					|    },
					|    {
					|      "_id": 2,
					|      "title": "Alice''s Adventures in Wonderland",
					|      "author": {
					|        "_id": 2,
					|        "name": "Lewis Carroll"
					|      },
					|      "publisher": {
					|        "_id": 2,
					|        "name": "Enhanced Media"
					|      }
					|    },
					|    {
					|      "_id": 3,
					|      "title": "The Adventures of Huckleberry Finn",
					|      "author": {
					|        "_id": 3,
					|        "name": "Mark Twain"
					|      },
					|      "publisher": {
					|        "_id": 3,
					|        "name": "Amazon Classics"
					|      }
					|    }
					|  ]
					|}
				""".trim.stripMargin)

		env.process( "GET", "/books?filter=title=The+Adventures+of+Huckleberry+Finn,books._id=3", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 3,
					|      "title": "The Adventures of Huckleberry Finn",
					|      "author": {
					|        "_id": 3,
					|        "name": "Mark Twain"
					|      },
					|      "publisher": {
					|        "_id": 3,
					|        "name": "Amazon Classics"
					|      }
					|    }
					|  ]
					|}
				""".trim.stripMargin)

		env.process( "GET", "/books?order=title:asc", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": [
					|    {
					|      "_id": 2,
					|      "title": "Alice''s Adventures in Wonderland",
					|      "author": {
					|        "_id": 2,
					|        "name": "Lewis Carroll"
					|      },
					|      "publisher": {
					|        "_id": 2,
					|        "name": "Enhanced Media"
					|      }
					|    },
					|    {
					|      "_id": 3,
					|      "title": "The Adventures of Huckleberry Finn",
					|      "author": {
					|        "_id": 3,
					|        "name": "Mark Twain"
					|      },
					|      "publisher": {
					|        "_id": 3,
					|        "name": "Amazon Classics"
					|      }
					|    },
					|    {
					|      "_id": 1,
					|      "title": "The Brothers Karamazov",
					|      "author": {
					|        "_id": 1,
					|        "name": "Fyodor Dostoyevsky"
					|      },
					|      "publisher": {
					|        "_id": 1,
					|        "name": "North Point Press"
					|      }
					|    }
					|  ]
					|}
				""".trim.stripMargin)

		env.process( "GET", "/books/1", null ) shouldBe
			(SC_OK, "application/json",
				"""
					|{
					|  "data": {
					|    "_id": 1,
					|    "title": "The Brothers Karamazov",
					|    "author": {
					|      "_id": 1,
					|      "name": "Fyodor Dostoyevsky"
					|    },
					|    "publisher": {
					|      "_id": 1,
					|      "name": "North Point Press"
					|    }
					|  }
					|}
				""".trim.stripMargin)
	}
}
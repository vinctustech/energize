documentation examples
======================

these examples show how to use Energize

get the "mini-northwind" database
---------------------------------

start the REPL with

	runMain xyz.hyperreal.energize.REPLMain
	
load the mini-northwind database

	l examples/mini-northwind
	
basic types
-----------

the *categories* resource is defined like this

    resource categories
        categoryName string
        description string

this resource has two fields `categoryName` and `description`, both of which are declared to be `string`.

get the categories of products the company carries

	get /categories
	
the JSON result is

	{
      "data": [
        {
          "_id": 1,
          "categoryName": "Beverages",
          "description": "Soft drinks, coffees, teas, beers, and ales"
        },
        {
          "_id": 2,
          "categoryName": "Condiments",
          "description": "Sweet and savory sauces, relishes, spreads, and seasonings"
        },
        {
          "_id": 3,
          "categoryName": "Confections",
          "description": "Desserts, candies, and sweet breads"
        },
        {
          "_id": 4,
          "categoryName": "Dairy Products",
          "description": "Cheeses"
        },
        {
          "_id": 5,
          "categoryName": "Grains/Cereals",
          "description": "Breads, crackers, pasta, and cereal"
        },
        {
          "_id": 6,
          "categoryName": "Meat/Poultry",
          "description": "Prepared meats"
        },
        {
          "_id": 7,
          "categoryName": "Produce",
          "description": "Dried fruit and bean curd"
        },
        {
          "_id": 8,
          "categoryName": "Seafood",
          "description": "Seaweed and fish"
        }
      ]
    }
    
record ID numbers are generated automatically. fields that are declared `string` or `text` are returned as a JSON string.

we can see that there are eight categories, but let's check using

	get /categories/count
	
returning

	{
	  "data": 8
	}

we could find the category that includes pasta

	get /categories?filter=description~%25pasta%25

# query parameters

filter =age<
        age>
        age=
        age!=
        age<=
        age>=

description~%25pasta%25
description~_

fields=age,description
(in order)

order=field:asc,fiedl2:desc

page=num&limit=num (default to 10)

# data types

boolean true, false
string ".*"
text ".*"
integer
long
uuid "xxxx-xxxx-4xxx-xxxx"
date ISODate
datetime ISODatetime
time ISO Time
timestamp ISO Timestamp
binary "313241"
blob  base64 encoded data
float java double IEEE 754
decimal (1,3)
media data url
enum

to get

    {
      "data": [
        {
          "_id": 5,
          "categoryName": "Grains/Cereals",
          "description": "Breads, crackers, pasta, and cereal"
        }
      ]
    }
    
the *products* resource is defined as

	resource products
        productName string
        supplier suppliers
        category categories
        unit string
        price decimal(20, 2)

the `enum` data type

    "enum" in {
        val (c, s, d) = Test.dbconnect
        val key = AUTHORIZATION.getString( "key" )
        val config =
            """
                |enum suit [clubs, diamonds, hearts, spades]
                |
                |resource cards
                |	suit suit
                |	number integer
            """.trim.stripMargin
        val env = Energize.configure( io.Source.fromString( config ), c, s, d, key )

        env.process( "POST", "/cards", """{suit: "hearts", number: 1}""" ) shouldBe
            (SC_CREATED, "application/json",
                """
                    |{
                    |  "data": 1
                    |}
                """.trim.stripMargin )
        env.process( "GET", "/cards", null ) shouldBe
            (SC_OK, "application/json",
                """
                    |{
                    |  "data": [
                    |    {
                    |      "_id": 1,
                    |      "suit": "hearts",
                    |      "number": 1
                    |    }
                    |  ]
                    |}
                """.trim.stripMargin )
        env.process( "PUT", "/cards/1", """{number: 2, suit: "clubs"}""" ) shouldBe (SC_NO_CONTENT, null, null)
        env.process( "GET", "/cards", null ) shouldBe
            (SC_OK, "application/json",
                """
                    |{
                    |  "data": [
                    |    {
                    |      "_id": 1,
                    |      "suit": "clubs",
                    |      "number": 2
                    |    }
                    |  ]
                    |}
                """.trim.stripMargin )
    }

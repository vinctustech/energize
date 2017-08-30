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


run the REPL

	runMain xyz.hyperreal.energize.REPLMain
	
load the mini-northwind database

	l examples/mini-northwind
	
basic types
-----------

the *shippers* resource is defined like this

	resource shippers
    	shipperName string
    	phone string

requesting

	get /shippers
	
returns

	{
	  "data": [
		{
		  "_id": 1,
		  "shipperName": "Speedy Express",
		  "phone": "(503) 555-9831"
		},
		{
		  "_id": 2,
		  "shipperName": "United Package",
		  "phone": "(503) 555-3199"
		},
		{
		  "_id": 3,
		  "shipperName": "Federal Shipping",
		  "phone": "(503) 555-9931"
		}
	  ]
	}

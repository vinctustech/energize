CRAS
====

Configurable ReST API Server

[![Build Status](https://travis-ci.org/edadma/cras.svg?branch=dev)](https://travis-ci.org/edadma/cras)


Overview
--------

*cras* (which is an acronym for "configurable ReST API server) allows you to get your ReST API up and running in very little time by reducing the amount of typing normally required.


Example
-------

This example shows how to get a simple API to support a "to do list" app working. Start by creating a folder for the example. Now download the [executable](https://dl.bintray.com/edadma/generic/cras-0.1.jar) and place it in the example folder you just created. Now, create a text file called `todo.cras` will the following text in it.

	table todo api/v1
	  name        string  required
	  description string  optional
	  status      integer required

Now, on the command line in the example folder, start the server with the command

	java -cp cras-0.1.jar xyz.hyperreal.cras.ServerMain todo todo
	
You should now have a working HTTP server bound to port 8080 that will serve API requests for a todo list. Let's try it out. Using `curl`, let's add an item to our todo list database. To do that, type

	curl --data "{name: \"first item\", status: 1}" http://localhost:8080/api/v1/todo

You should see

	{
		"status": "ok",
		"update": 1
	}

as the response. This means that the route (URI) was correct and that one row was added to the database. We can see our todo list with the command

	curl http://localhost:8080/api/v1/todo
	
getting the response

	{
		"status": "ok",
		"data": [
			{
				"ID": 1,
				"NAME": "first item",
				"DESCRIPTION": null,
				"STATUS": 1
			}
		]
	}

The description is `null` because we marked that column as `optional` and did not provide data for it in our post.
CRAS [![Build Status](https://travis-ci.org/edadma/cras.svg?branch=0.5)](https://travis-ci.org/edadma/cras) [![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
====

Configurable ReST API Server


Overview
--------

*cras* (which is an acronym for "configurable ReST API server") allows you to get your ReST API up and running in very little time by drastically reducing the amount of typing normally required.


License
-------

*cras* is distributed under the MIT License, meaning that you are free to use it in your free or proprietary software.


Documentation
-------------

- [Example inside this README](http://github.com/edadma/cras#example)
- [CRAS Reference Manual](http://edadma.github.io/cras)
- Type `java -jar cras-0.5.jar --help` for executable options
- Type `help` inside the REPL for commands


Usage
-----

### Executable

If you just want to download the executable so that you can have an API server for your project or use the REPL, you can download it from [here](https://dl.bintray.com/edadma/generic/cras-0.5.jar). *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 8+ installed.

Run it as a normal Java executable JAR with the command `java -jar cras-0.5.jar <config>` in the folder where you downloaded the file, where *config* is the name of the `.cras` file (without the file extension) that defines your API.

### Library

Use the following definition to use *cras* in your Maven project:

	<repository>
		<id>hyperreal</id>
		<url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
		<groupId>xyz.hyperreal</groupId>
		<artifactId>cras</artifactId>
		<version>0.5</version>
	</dependency>

Add the following to your `build.sbt` file to use *cras* in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "xyz.hyperreal" %% "cras" % "0.5"


Building
--------

### Requirements

- Java 8+
- SBT 0.13.13+
- Scala 2.12.1+

### Clone and Run the SBT (Simple Build Tool)

	git clone git://github.com/edadma/cras.git
	cd cras
	sbt
	
### SBT Commands

Once the SBT prompt appears, use the following commands.

- `test` builds and executes tests
- `re-start <config>` starts the server where *config* is the name of the `.cras` file (without the file extension) that defines your API. You can press ENTER once the server has started to get the SBT prompt in order to enter SBT commands while the server is running.
- `re-stop` stops the server. The port will be unbound some time after the server process has been killed (on Linux).
- `runMain xyz.hyperreal.cras.REPLMain` starts the REPL
- `run <config>` this will also start the server, however, it is recommended to use `re-start` instead. Starting the server using `run` means that the JVM process that SBT is running in is the one that is bound to the port that the server is using, whereas using `re-start` causes the server to run in a separate process that can later be killed using `re-stop`.


Example
-------

This example shows how to get a simple API to support a "to do list" app working. Start by creating a folder for the example. Now download the [executable](https://dl.bintray.com/edadma/generic/cras-0.5.jar) and place it in the example folder you just created. Now, create a text file called `todo.cras` will the following text in it.

	resource todo /api/v1
	  name        string  required
	  description string  optional
	  status      integer required

The executable contains both an HTTP server and a REPL to make it easier to develop your API definitions. We'll start by looking at the server.


### HTTP Server

Now, on the command line in the example folder, start the server with the command

	java -jar cras-0.5.jar todo
	
You should now have a working HTTP server bound to port 8080 that will serve API requests for a todo list. Let's try it out. Using `curl`, let's add an item to our todo list database. To do that, type

	curl --data "{name: \"finish 0.1\", status: 1}" http://localhost:8080/api/v1/todo

You should see

	{
		"status": "ok",
		"update": 1
	}

as the response. This means that the route (URI) was correct and that one row was added to the database. Add another item by typing

	curl --data "{name: \"write readme\", description: \"add example involving finishing 0.1 and writing the readme\", status: 1}" http://localhost:8080/api/v1/todo

We can see our todo list with the command

	curl http://localhost:8080/api/v1/todo
	
getting the response

	{
		"status": "ok",
		"data": [
			{
				"id": 1,
				"name": "finish 0.1",
				"description": null,
				"status": 1
			},
			{
				"id": 2,
				"name": "write readme",
				"description": "add example involving finishing 0.1 and writing the readme",
				"status": 1
			}
		]
	}

The `description` is `null` in the first one because we marked that column as `optional` and did not provide data for it in our post. Also, notice that the property names are the column names originally provided in the configuration and not the case-insensitive uppercase names that are returned by the database. We can retrieve just the second item with the command

	curl http://localhost:8080/api/v1/todo/2
	
to get

	{
		"status": "ok",
		"data": {
			"id": 2,
			"name": "write readme",
			"description": "add example involving finishing 0.1 and writing the readme",
			"status": 1
		}
	}

Notice that with this response, the `data` field is not an array but a single object since the URI path is pointing to a single item within the `todo` resource.  Lastly, if we try querying the server for an item that does not exist from a resource that does exist, we should get a `404` status code from the server because what is being requested doesn't exist. Try it

	curl http://localhost:8080/api/v1/todo/3

Press `Ctrl-C` to stop the server.


### REPL

To start the REPL, type the following command (while in the same folder where the executable was placed)

	java -cp cras-0.5.jar xyz.hyperreal.cras.REPLMain
	
By default, the REPL creates an in-memory H2 database for you and connects to it. Type `help` to see all the REPL commands. Tell the REPL to load the `todo` configuration by typing

	l todo
	
To verify that a table called `todo` has been created, type the SQL command

	select * from todo;
	
To use your Postgres database specified in your database configuration instead type the following commands

	config
	connect
	
In the REPL, you can always restart from scratch using the `wipe` command, reload a modified configuration using `load`, etc.

# Energize

[![Build Status](https://travis-ci.org/vinctustech/energize.svg?branch=master)](https://travis-ci.org/vinctustech/energize) [![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://opensource.org/licenses/ISC)

Create a Rest API in Seconds

Overview
--------

*Energize* allows you to get your Rest API server and database up and running in very little time.


License
-------

*Energize* is distributed under the ISC License, meaning that you are free to use it in your free or proprietary software.


Documentation
-------------

- [Get Started](http://vinctustech.github.io/energize)
- [Scaladoc](http://vinctustech.github.io/energize/api)
- [Example inside this README](http://github.com/vinctustech/energize#example)
- Type `java -jar energize-0.11.jar --help` for executable options
- Type `help` inside the REPL for commands


Usage
-----

### Executable

If you just want to download the executable so that you can have an API server for your project or use the REPL, you can download it from [here](https://dl.bintray.com/edadma/generic/energize-0.11.jar). *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 8+ installed.

Run it as a normal Java executable JAR with the command `java -jar energize-0.11.jar <config>` in the folder where you downloaded the file, where *config* is the name of the `.energize` file (without the file extension) that defines your API.

### Library

Use the following definition to use *Energize* in your Maven project:

	<repository>
	  <id>hyperreal</id>
	  <url>https://dl.bintray.com/edadma/maven</url>
	</repository>
	 
	<dependency>
	  <groupId>xyz.hyperreal</groupId>
	  <artifactId>energize</artifactId>
	  <version>0.11</version>
	</dependency>
	
Add the following to your `build.sbt` file to use *Energize* in your SBT project:

    resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"
	 
    libraryDependencies += "xyz.hyperreal" %% "energize" % "0.11"


Building
--------

### Requirements

- Java 8 (any revision)
- SBT 0.13.13+ (which then downloads Scala 2.12.x automatically)

### Clone and Run the SBT (Simple Build Tool)

	git clone git://github.com/vinctustech/energize.git
	cd energize
	sbt
  
### SBT Commands

Once the SBT prompt appears, use the following commands.

- `test` builds and executes tests
- `re-start <config>` starts the server where *config* is the name of the `.energize` file (without the file extension) that defines your API. You can press ENTER once the server has started to get the SBT prompt in order to enter SBT commands while the server is running.
- `re-stop` stops the server. The port will be unbound some time after the server process has been killed (on Linux).
- `runMain xyz.hyperreal.energize.REPLMain` starts the REPL
- `run <config>` this will also start the server, however, it is recommended to use `re-start` instead. Starting the server using `run` means that the JVM process that SBT is running in is the one that is bound to the port that the server is using, whereas using `re-start` causes the server to run in a separate process that can later be killed using `re-stop`.


Example
-------

This example shows how to get a simple API to support a "to do list" app working. Start by creating a folder for the example. Now download the [executable](https://dl.bintray.com/edadma/generic/energize-0.11.jar) and place it in the example folder you just created. Now, create a text file called `todo.energize` with the following text in it.

	resource todo /api/v1
	  name        string  required
	  description string  optional
	  status      integer required

The executable contains both an HTTP server and a REPL to make it easier to develop your API definitions. We'll start by looking at the server.


### HTTP Server

Now, on the command line in the example folder, start the server with the command

    java -jar energize-0.11.jar todo
  
You should now have a working HTTP server bound to port 8080 that will serve API requests for a todo list. Let's try it out. Using `curl`, let's add an item to our todo list database. To do that, type

	curl -d "{name: \"finish 0.1\", status: 1}" http://localhost:8080/api/v1/todo

You should see

	{
	  "data": 1
	}

as the response. This means that the route (URI) was correct and that one row was added to the database. Add another item by typing

	curl -d "{name: \"write readme\", description: \"add example involving finishing 0.1 and writing the readme\", status: 1}" http://localhost:8080/api/v1/todo

We can see our todo list with the command

	curl http://localhost:8080/api/v1/todo
  
getting the response

	{
	  "data": [
	    {
	      "_id": 1,
	      "name": "finish 0.1",
	      "description": null,
	      "status": 1
	    },
	    {
	      "_id": 2,
	      "name": "write readme",
	      "description": "add example involving finishing 0.1 and writing the readme",
	      "status": 1
	    }
	  ]
	}

The `description` is `null` in the first one because we marked that column as `optional` and did not provide data for it in our post. Also, notice that the property names are the column names originally provided in the configuration and not the case-insensitive uppercase names that are returned by the database (H2 by default). We can retrieve just the second item with the command

	curl http://localhost:8080/api/v1/todo/2
  
to get

	{
	  "data": {
	    "_id": 2,
	    "name": "write readme",
	    "description": "add example involving finishing 0.1 and writing the readme",
	    "status": 1
	  }
	}

Notice that with this response, the `data` field is not an array but a single object since the URI path is pointing to a single item within the `todo` resource.  Lastly, if we try querying the server for an item that does not exist from a resource that does exist, we should get a `404` status code from the server because what is being requested doesn't exist. Try it

    curl http://localhost:8080/api/v1/todo/3

Press `Ctrl-C` to stop the server.


### REPL

To start the REPL (read, execute, print, loop), type the following command (while in the same folder where the executable was placed)

	java -cp energize-0.11.jar xyz.hyperreal.energize.REPLMain
  
By default, the REPL creates an in-memory H2 database for you and connects to it. Type `help` to see all the REPL commands. Tell the REPL to load the `todo` configuration by typing

	l todo
  
To verify that a table called `todo` has been created, type the SQL command

	select * from todo;
  
To use your Postgres database specified in your database configuration instead type the following commands

	config
	connect
  
In the REPL, you can always restart from scratch using the `wipe` command, reload a modified configuration using `load`, etc.
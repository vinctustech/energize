CRAS
====

Configurable ReST API Server

[![Build Status](https://travis-ci.org/edadma/cras.svg?branch=dev)](https://travis-ci.org/edadma/cras)


Overview
--------

*cras* (which is an acronym for "configurable ReST API server) allows you to get your ReST API up and running in very little time by reducing the amount of typing normally required.


Example
-------

This example shows how to get a simple API to support a "to do list" app working. Start by creating a folder for the example. Now download the [executable](https://dl.bintray.com/edadma/generic/:cras-0.1.jar) and place it in the example folder you just created. Now create a text file called `todo.cras` will the following text in it.

	table todo api/v1
	  name        string  required
	  description string  optional
	  status      integer required


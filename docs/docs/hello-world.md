---
id: hello-world
title: Hello World
permalink: docs/hello-world.html
prev: installation.html
next: default-routes.html
---

## Hello World

First, make sure you have the latest version of Energize and clone the current working repository.

```
$ git clone git://github.com/vinctustech/energize.git
$ cd energize
```

For the classic `Hello World!` example we will define a resource that could be used in a simple messenger REST API application. Create a file in the root of the project called `messenger.energize` with the following `resource` definition:

```
resource messages
  text string
```

The messages `resource` has one field called `text`, and this field has a data type of `string`. Fields need to be indented for Energize to properly parse the file.

> Note: All resources have a default `id` field (with `integer` as its data type) that is automatically incremented by Energize.

### Data types
```
string, integer, uuid, date, long, datetime, time, timestamp
```

If we wanted the `text` field to be indexed, we can add the `indexed` modifier.

```
resource messages
  text string indexed
```

### Modifiers

```
indexed, unique, required, optional, secret
```

In the root directory `energize`, run the project with `sbt`.

```
$ sbt
```

After the SBT prompt appears, you can start the server using the `messenger.energize` file:

```
$ re-start messenger
```

Now we can `POST` a message to our REST API:

```
POST http://localhost:8080/messages HTTP/1.1

{
  "text": "Hello World!"
}
```

This will respond with a JSON result giving the `id` of the created document:

```
{
  "data": 1
}
```
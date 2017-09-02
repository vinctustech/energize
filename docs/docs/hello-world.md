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

For the classic `Hello World!` example we will define a resource that could be used in a simple messenger application. Create a file in the root of the project called `messenger.energize` with the following `resource` definition:

```
resource messages
  text string
```

The messages `resource` has one field called `text`, and this field has a data type of `string`. Fields need to be indented for Energize to properly parse the file.

> Note: All resources have a default `_id` field (with `integer` as its data type) that is automatically incremented by Energize.

### Primitive Data types

| Type          | Description                            | Example Literals                                |
|:--------------|:---------------------------------------|:------------------------------------------------|
| **boolean**   | true or false                          | true, false                                     |
| **string**    | sequence of characters                 | "Hello World!"                                  |
| **text**      | large sequence of characters           | "Far far away.."                                |
| **integer**   | twos complement integer                | -2, -1, 0, 1, 2                                 |
| **uuid**      | RFC 4122 universally unique identifier | "703d3398-f533-4bec-a367-64734344da24"          |
| **date**      | ISO 8601 date format                   | "2017-09-02"                                    |
| **datetime**  | ISO 8601 datetime format               | "2017-09-02T14:32:00"                           |
| **time**      | ISO 8601 time format                   | "14:32:00"                                      |
| **binary**    | hexadecimal strings                    | "48656c6c6f20576f726c64"                        |
| **blob**      | base64 encoded data                    | "SGVsbG8gV29ybGQ="                              |
| **float**     | IEEE 754 binary64 floating point value | 3.14159                                         |
| **decimal**   | exact numeric                          | 4.99                                            |
| **media**     | data url                               | data:text/plain;base64,aGVsbG8=                 |
| **enum**      | set of named values                    | [Hearts, Spades, Clubs, Diamonds]               |

If we want the `text` field to be indexed, we can add the `indexed` modifier.

```
resource messages
  text string indexed
```

### Modifiers

| Type          | Description                                      |
|:--------------|:-------------------------------------------------|
| **indexed**   | creates a unique index                           |
| **unique**    | ensures that all values in a field are different |
| **required**  | enfroces a field to NOT accept null values       |
| **optional**  | allows a field to accept null values             |
| **secret**    | hides a field value from being retrieved         |

In the root directory of `energize`, run the project with `sbt`.

```
$ sbt
```

After the SBT prompt appears, start the server using the `messenger.energize` file:

```
$ re-start messenger
```

Now we can `POST` a message to our REST API:

```
curl http://localhost:8080/messages \
	-X POST \
	-H 'Content-Type: application/json' \
	-d '{"text": "Hello world!"}'
```

This will respond with a JSON result giving the `id` of the created document:

```
{
  "data": 1
}
```
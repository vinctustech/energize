---
id: relationships
title: Relationships
permalink: docs/relationships.html
prev: pagination.html
next: access-control.html
---

## Relationships

Let's say we have defined three resources, `books`, `authors`, and `publishers`. In the following sections, we will learn how to make relationships between these resources.

```
resource books
  title string required unique
  description string optional

resource authors
  name string required

resource publishers
  name string required
```

### One-to-many

A one-to-many relationship is defined by using a resource's name as the data type for a field.

```
resource books
  publisher publishers
  // ...
```

To create the relationship, create a document and use the `id` of a related document as the value of the field.

```
curl http://localhost:8080/books \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"title": "Dune: House Atreides", "publisher": 1}'
```

Here, the `publisher` field has a value of `1` (the `id` of the related document).

### Many-to-many

A many-to-many relationship is defined by using a resource's name as the data type, followed by the `array` modifier.

```
resource books
  authors authors array
  // ...
```

To create the relationships, first create a document, and then populate it's relationships. The URI should be that of the resource end-point, followed by the `id`, and then the name of the resource we are creating relationships for.

```
curl http://localhost:8080/books/1/authors \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"name": "Brian Herbert"}'
```

And again...

```
curl http://localhost:8080/books/1/authors \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"name": "Kevin J. Anderson"}'
```

Now retrieve our book by `id`, to get all related data.

```
curl http://localhost:8080/books/1
```

If successful, this should return a `200 OK` with the following json.

```
{
  "data": {
    "title": "Dune: House Atreides",
    "description": null,
    "authors": [
      {
        "id": 1,
        "name": "Brian Herbert"
      },
      {
        "id": 2,
        "name": "Kevin J. Anderson"
      }
    ],
    "publisher": {
      "id": 1,
      "name": "Spectra"
    },
    "id": 1
  }
}
```

### Tables

A one-to-many, or many-to-many relationship can also be made with a table, rather than a resource. Tables are not accessible by default routes, but are useful for storing data that can be used by different resources.

Tables are defined by using the `table` keyword.

```
resource citizen
  name string required
  country countries required

table countries
  name string required
```
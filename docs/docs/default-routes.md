---
id: default-routes
title: Default Routes
permalink: docs/default-routes.html
prev: hello-world.html
next: pagination.html
---

## Default Routes

Let's say we have defined a resource called `messages`.

```
resource messages
  text string indexed
```

The default routes for `CRUD` operations on the resource will be handled automatically by Energize.

> Note: The following examples assume you are running Energize on port `8080`.

### Create a document

To create a document, make a `POST` request at the resources end-point.

```
POST http://localhost:8080/messages HTTP/1.1
Content-Type: application/json

{
  text: "Hello World!"
}
```

If successful, this will respond with a `201 Created` and include a `JSON` body with the created document's `id`.

```
HTTP/1.1 201 Created
Content-Type: application/json

{
  "data": 1
}
```

### Retrieve a document

To retrieve a document, make a `GET` request at the resources end-point followed by the `id` of the required document.

```
GET http://localhost:8080/messages/1 HTTP/1.1
```

If the document exists, you will recieve a `200 OK` with the document's data included as `JSON`.

```
{
  "data": {
    "id": 1,
    "text": "Hello World!"
  }
}
```

### Update a document

To update a document, make a `PUT` request at the resources end-point followed by the `id` of the document to be updated, and include all the required fields to be updated as `JSON`.

```
PUT http://localhost:8080/messages/1 HTTP/1.1
Content-Type: application/json

{
  "text": "Hello World! Goodbye World."
}
```

If successful, you will receive a `204 No Content`, meaning it has successfully updated the document, but there is no data in the response body.

> Note: `id` values are immutable and should not be in the `JSON` body of an update request.

### Delete a document

To delete a document, make a `DELETE` request at the resources end-point followed by the `id` of the document you want to delete.

```
DELETE http://localhost:8080/messages/1 HTTP/1.1
```

If successful, you will receive a `204 No Content`, meaning it has successfully deleted the document, but there is no data in the response body.

### List all documents

Assuming you have created many `messages` documents, and you want to list all of them. To achieve this, make a `GET` request at the resource end-point.

```
GET http://localhost:8080/messages HTTP/1.1
```

If successful, you will recieve a `200 OK` with an array of the resources as `JSON`.

```
HTTP/1.1 200 OK

{
  "data": [
  {
    "id": 2,
    "text": "Hey Mary"
  },
  {
    "id": 3,
    "text": "Hi John, how are you?"
  },
  {
    "id": 4,
    "text": "Not bad, just learning Energize!"
  }
  ]
}
```
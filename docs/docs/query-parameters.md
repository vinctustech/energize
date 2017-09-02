---
id: query-parameters
title: Query Parameters
permalink: docs/query-parameters.html
prev: default-routes.html
next: relationships.html
---

## Query Parameters

Let's say we have defined a `messages` resource.

```
resource messages
  text string indexed
  createdAt datetime required
```

There are default query parameters that can be used on resources.

### Pagination
To paginate over a list of documents use the `page` and `limit` query parameters.

```
curl http://localhost:8080/messages?page=2&limit=5
```

This will retrieve 5 documents on the second page of results.

> Note: If you only pass the `page` query parameter, `limit` will be set to 10 by default.

### Fields

Sometimes we may only need certain fields in our result data. To achieve this, we can pass the `fields` query parameter, which will only return the specified fields.

```
curl http://localhost:8080/messages?page=2&limit=5&fields=text
```

This will retrieve 5 documents on the second page of results, and only return the `text` field as `JSON` in the body.

> Note: If multiple fields are provided, retrieved items will have their fields appear the in the same order.

### Order

To sort a list of results, use the `order` query parameter. The `order` query parameter takes a field name, followed by a colon, then can either take `asc` or `desc`.

```
curl http://localhost:8080/messages?order=text:asc
```

This will sort the result list by the `text` field in ascending order.

> Note: If you can pass multiple arguments by comma seperating them.

### Count

To return the total number of documents of a resource, use the `count` path parameter on the resource end-point.

```
curl http://localhost:8080/messages/count
```

This will return the total number of documents of the specified resource.
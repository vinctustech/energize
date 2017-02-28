---
id: access-control
title: Access Control
permalink: docs/access-control.html
prev: relationships.html
next: action-script.html
---

## Access Control

Users are prebuilt into Energize in the default `users` resource. Resources can be protected by either basic authentication, or bearer token authentication. You can then choose to implement either type of authentication method in your client.

The following users definition is hard-coded into Energize, and does not need to be included in your definitons.

```
resource users protected (admin)
  email string unique
  createdTime timestamp
  updatedTime timestamp
  state integer
  groups string array
  password string secret
```

To add custom fields to the users resource, overload the users definition with as many fields as needed.

```
resource users
  firstName string optional
  lastName string optional
```

### User onboarding

To handle registration, use the default registration route.

```
POST http://localhost:8080/auth/register HTTP/1.1
Content-Type: application/json

{
  "email": "test@example.com",
  "password": "not-so-secret"
}
```

To handle login, use the default login route.

```
POST http://localhost:8080/auth/login HTTP/1.1
Content-Type: application/json

{
  "email": "test@example.com",
  "password": "not-so-secret"
}
```

If successful, you will get a `200 OK` with an access token in the response json.

```
{
  "data": "jtwmaswrjwrooar"
}
```

This access token can be used on every subsequent request in the `Authorization` header as a bearer token to authenticate the user on protected resources.

```
POST http://localhost:8080/some-protected-resource HTTP/1.1
Content-Type: application/json
Authorization: Bearer arjcyaiprkealtj

{
  "data": "some-random-data"
}
```

> Note: On registration, an access token is also given in the response json.

### Protecting resources

To protect a resource you can used the `protected` resource modifier, followed by the groups that are allowed access on that resource. The two default groups are `users`, and `admin`. Those who have created an account with the `users` resource have the `users` group by default.

> Note: You can edit the default groups in the `src/main/resources/reference.conf` file.

```
resource publishers protected (users, admin)
	string name required
```
---
id: access-control
title: Access Control
permalink: docs/access-control.html
prev: relationships.html
next: action-script.html
---

## Access Control

Users are prebuilt into Energize in the default `users` resource. The `users` resource is protected and can only be accessed by users with the `admin` group.

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

To authenticate you can either use basic, or bearer token authentication.

### Protecting Resources

### Basic Authentication

### Bearer Authentication
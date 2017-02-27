---
id: relationships
title: Relationships
permalink: docs/relationships.html
prev: pagination.html
next: access-control.html
---

## Relationships

Let's say we have defined three resources, `books`, `authors`, and `publishers`.

```
resource books
	title string required unique
	description string optional
	authors authors array
	publisher publishers

resource authors
	name string required

resource publishers
	name string required
```


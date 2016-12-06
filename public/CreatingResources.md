# Defining a Resource

Let's say we want to define a resource for users. Here's an example of a `.cras` file of what that would look like.

	resource users
	  unique required string email
	  required string password
	  optional string name
	  integer age

Our example `users` resource definition specifies four properties, each property corresponding to a piece of data. Properties have modifiers (`optional`, `required`, `unique`, `index`), followed by a type (in this example we have `string` and `integer`), and a name. If no modifiers are specified, the `optional` modifier is used by default.

There is no need to include an id field, since ids are managed for us automatically. Each resource has a default id property.

# One-to-one Relationship

So far we've only seen primitive types for our properties, but we can also have composite types. A composite type allows us to create a relationship for another resource by using the name of the referenced resource as a type.

Let's say we have another resource called `roles`.

	resource roles
		required string name

If we want our `users` resource to have a relationship with our `roles` resource, we simply include it as a composite type and give it a name. This allows us to represent a data model with a many-to-one relationship.

	table users
	  // ...
	  roles role

# One-to-many Relationship

Sometimes we might want to give multiple roles to a user. To achieve this we can use the `array` modifier. This allows us to represent a one-to-many relationship.

	table users
	  // ...
	  array roles roles

> Note: we can *not* use the `array` modifier on scalar types.

# Many-to-many Relationship

What if we needed to represent a data model that required a many-to-many relationship? For instance, we have a resource of `books` and another of `authors`, where a book can have multiple authors and an author can write multiple books. Normally, in our database schema we'd model this with a `books` table, an `authors` table, and then have a junction table that had rows for the relationships.

We can model this with the following example.

	resource books
		// ...
		array authors authors

	resource authors
		// ...
		array books books

The junction table is automagically created for us.
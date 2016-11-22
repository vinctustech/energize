# Defining a Table

Let's say we want to define a table for users. Here's an example of a `.info` file of what that would look like.

	table users
	  unique required string email
	  required string password
	  optional string name
	  integer age

Our example `users` table definition specifies four fields, each field corresponding to a piece of data. Fields have modifiers (`optional`, `required`, `unique`), followed by a type (in this example we have `string` and `integer`), and a name. If no modifiers are specified, the `optional` modifier is used by default.

There is no need to include an id field, since ids are managed for us automatically. Each `table` has a default id field which looks like a UUID.

So far we've only seen scalar types, but we can also have composite types. A composite type allows us to reference a row from another table by using the name of the referenced table as a type.

Let's say we have another table called `roles`, and it looked like this.

	table roles
		required string name

If we want our `users` table to have a field that references a single role from our `roles` table, we simply include it as a composite type and give it a name.

	table users
	  // ...
	  roles role

Sometimes we might want to give multiple roles to a user. To achieve this we can use the `array` modifier.

	table users
	  // ...
	  array roles roles

> Note: we can *not* use the `array` modifier on scalar types.
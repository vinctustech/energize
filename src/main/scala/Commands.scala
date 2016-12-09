package xyz.hyperreal.cras


object Commands {
	def command( env: Env, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Env, resource: Table, id: String ) = command( env, s"delete from ${resource.name} where id = '$id';" )
}
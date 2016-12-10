package xyz.hyperreal.cras


object CommandFunctions {
	def command( env: Env, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Env, resource: Table, id: String ) = command( env, s"delete from ${resource.name} where id = '$id';" )
	
	def insert( env: Env, resource: Table, json: Map[String, Any] ) = {
		val com = new StringBuilder( "insert into " )
		val last = resource.names.last
		
		com ++= resource.name
		com ++= resource.names.mkString( "(", ", ", ") values(" )
		
		for (c <- resource.names) {
			json get c match {
				case None => com ++= "null"
				case Some( v ) =>
					if (resource.columns(c.toUpperCase).typ == StringType) {
						com += '\''
						com ++= String.valueOf( v )
						com += '\''
					} else
						com ++= String.valueOf( v )
			}
			
			if (c != last)
				com ++= ", "
		}
		
		com += ')'				
		env.statement.executeUpdate( com.toString )
	}
	
	def update( env: Env, resource: Table, json: Map[String, Any], id: Int, all: Boolean ) = {
		if (all && json.keySet != (resource.columns.keySet - "id"))
			throw new CrasErrorException( "update: missing column(s) in PUT request" )
		else {
			val com = new StringBuilder( "update " )
			val last = resource.names.last
			
			com ++= resource.name
			com ++= " set "
			com ++=
				json.toList map {
					case (k, v) if resource.columns(k.toUpperCase).typ == StringType => k + "='" + String.valueOf( v ) + "'"
					case (k, v) => k + "=" + String.valueOf( v )
				} mkString ", "
			com ++= " where id="
			com ++= id.toString
			env.statement.executeUpdate( com.toString )
		}
	}
}
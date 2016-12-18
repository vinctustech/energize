package xyz.hyperreal.cras


object CommandFunctions {
	def command( env: Env, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Env, resource: Table, id: Long ) = command( env, s"DELETE FROM ${resource.name} WHERE id = $id;" )
	
	def insert( env: Env, resource: Table, json: Map[String, AnyRef] ) = {
		val com = new StringBuilder( "INSERT INTO " )
		
		com ++= resource.name
		com ++= resource.names.mkString( " (", ", ", ") " )
		
		var sel = false
		var reft: String = null
		var refv: AnyRef = null
		val values =
			(for (c <- resource.names)
				yield {
					json get c match {
						case None => "NULL"
						case Some( v ) =>
							resource.columns(c.toUpperCase).typ match {
								case TableType( t ) if v != null && !v.isInstanceOf[Int] && !v.isInstanceOf[Long] =>
									sel = true
									reft = t
									refv = v
									"id"
								case StringType => '\'' + String.valueOf( v ) + '\''
								case _ => String.valueOf( v )
							}
					}
				}) mkString ", "
		
		com ++= (if (sel) "SELECT " else "VALUES (")
		com ++= values
		
		if (sel) {
			com ++= " FROM "
			com ++= reft
			com ++= " WHERE "
			com ++=
				(env.tables(reft.toUpperCase).columns.values.find( c => c.unique ) match {
					case None => throw new CrasErrorException( "insert: no unique column in referenced resource in POST request" )
					case Some( c ) => c.name
				})
			com ++= " = '"
			com ++= String.valueOf( refv )
			com += '\''
		}	else
			com += ')'
		
//		println( com )
		env.statement.executeUpdate( com.toString )
		
		val g = env.statement.getGeneratedKeys
		
		g.next
		g.getLong(1)
	}
	
	def update( env: Env, resource: Table, json: Map[String, Any], id: Long, all: Boolean ) =
		if (all && json.keySet != (resource.columns.keySet - "id"))
			throw new CrasErrorException( "update: missing column(s) in PUT request" )
		else {
			val com = new StringBuilder( "UPDATE " )
			var typ: ColumnType = null
			
			com ++= resource.name
			com ++= " SET "
			com ++=
				json.toList map {
					case (k, v) if resource.columns(k.toUpperCase).typ == StringType => k + " = '" + String.valueOf( v ) + "'"
					case (k, v) if {typ = resource.columns(k.toUpperCase).typ; typ.isInstanceOf[TableType]} =>
						if (v.isInstanceOf[Int] || v.isInstanceOf[Long])
							k + " = " + String.valueOf( v )
						else {
							val reft = typ.asInstanceOf[TableType].table
							val refc =
								(env.tables(reft.toUpperCase).columns.values.find( c => c.unique ) match {
									case None => throw new CrasErrorException( "update: no unique column in referenced resource in PUT/PATCH request" )
									case Some( c ) => c.name
								})

							k + s" = SELECT id FROM $reft WHERE $refc = '$v'"
						}
					case (k, v) => k + " = " + String.valueOf( v )
				} mkString ", "
			com ++= " WHERE id = "
			com ++= id.toString
			env.statement.executeUpdate( com.toString )
		}
}
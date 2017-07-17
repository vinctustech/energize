package xyz.hyperreal.energize


object AggragateFunctionsHelpers {

	def filtering( filter: Option[String] ) =
		QueryFunctionHelpers.filtering( filter ) match {
			case "" => ""
			case w => s"WHERE $w"
		}

	def checkField( resource: Table, field: String ) =
		resource.columnMap get field match {
			case None => throw new NotFoundException( s"field '$field' not found" )
			case Some( c ) => c.typ
		}

}

object AggragateFunctions {

	def count( env: Environment, resource: Table, filter: Option[String] ) = {
		val where = AggragateFunctionsHelpers.filtering(filter)
		val res = env.statement.executeQuery( s"SELECT COUNT(*) FROM ${resource.name} $where" )

		res.next

		BigInt( res.getLong(1) )
	}

	def sum( env: Environment, resource: Table, filter: Option[String], field: String ) = {
		val typ = AggragateFunctionsHelpers.checkField( resource, field )
		val where = AggragateFunctionsHelpers.filtering(filter)
		val res = env.statement.executeQuery( s"SELECT SUM(${nameIn(field)}) FROM ${resource.name} $where" )

		res.next

		typ match {
			case IntegerType => BigInt( res.getLong(1) )
			case FloatType => res.getDouble( 1 )
		}

	}

}
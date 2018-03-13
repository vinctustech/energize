package xyz.hyperreal.energize2


object AggregateFunctionHelpers {

	def checkField( resource: Resource, field: String ) =
		resource.fieldMap get field match {
			case None => throw new NotFoundException( s"field '$field' not found" )
			case Some( c ) => c.typ
		}

	def aggregate( func: String, resource: Resource, filter: Option[String], field: String ) = QueryFunctionHelpers.synchronized {
		val typ = AggregateFunctionHelpers.checkField( resource, field )
		val where = QueryFunctionHelpers.filtering( filter )
		val res = resource.statement.executeQuery( s"SELECT $func(${nameIn(field)}) FROM ${resource.name} $where" )

		res.next

		typ match {
			case IntegerType => BigInt( res.getLong(1) )
			case FloatType => res.getDouble( 1 )
		}
	}

}
/*
object AggregateFunctions {

	def count( vm: VM, resource: Resource, filter: Option[String] ) = QueryFunctionHelpers.synchronized {
		val where = QueryFunctionHelpers.filtering( filter )
		val res = resource.statement.executeQuery( s"SELECT COUNT(*) FROM ${resource.name} $where" )

		res.next

		BigInt( res.getLong(1) )
	}

	def avg( vm: VM, resource: Resource, filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( vm, "AVG", resource, filter, field )

	def sum( vm: VM, resource: Resource, filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( vm, "SUM", resource, filter, field )

	def min( vm: VM, resource: Resource, filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( vm, "MIN", resource, filter, field )

	def max( vm: VM, resource: Resource, filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( vm, "MAX", resource, filter, field )

}*/
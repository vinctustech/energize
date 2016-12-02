package xyz.hyperreal.cras

import java.sql._


case class Env( tables: Map[String, Table], routes: List[Route], results: FunctionExpression, variables: Map[String, String],
	connection: Connection, statement: Statement ) {

	def add( k: String, v: String ) = Env( tables, routes, results, variables + (k -> v), connection, statement )
	
	def add( m: Map[String,String] ) = Env( tables, routes, results, variables ++ m, connection, statement )

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )

case class Table( name: String, names: List[String], columns: Map[String, Column] )

case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean )

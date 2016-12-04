package xyz.hyperreal.cras

import java.sql._


case class Env( tables: Map[String, Table], routes: List[Route], variables: Map[String, Any], connection: Connection, statement: Statement ) {

	def add( kv: (String, Any) ) = Env( tables, routes, variables + kv, connection, statement )
	
	def add( m: Map[String, Any] ) = Env( tables, routes, variables ++ m, connection, statement )
	
	def lookup( name: String ) =
		variables get name match {
			case None =>
				tables get name.toUpperCase match {
					case None => throw new CrasErrorException( "variable not found: " + name )
					case Some( v ) => v
				}
			case Some( v ) => v
		}

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )

case class Table( name: String, names: List[String], columns: Map[String, Column] )

case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean )

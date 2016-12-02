package xyz.hyperreal.cras

import java.sql._


class Env( val routes: List[Route], val tables: Map[String, Table], val connection: Connection, val statement: Statement ) {

	

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )

case class Table( name: String, names: List[String], columns: Map[String, Column] )

case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean )

package xyz.hyperreal.informatio


trait AST


trait StatementAST extends AST

case class TableAST( name: String, fields: List[FieldAST] ) extends StatementAST
	

trait FieldType

case object StringType extends FieldType

case class FieldAST( typ: FieldType, name: String ) extends AST
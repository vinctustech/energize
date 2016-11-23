package xyz.hyperreal.informatio


trait AST


trait StatementAST extends AST

case class TableAST( name: String, fields: List[FieldAST] ) extends StatementAST
	

case class FieldAST( modifiers: List[FieldTypeModifier], typ: FieldType, name: String ) extends AST


trait FieldType

case object StringType extends FieldType


trait FieldTypeModifier

case object UniqueModifier extends FieldTypeModifier
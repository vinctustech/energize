package xyz.hyperreal.informatio


trait AST


trait StatementAST extends AST

case class TableDefinitionAST( name: String, fields: List[TableField] ) extends StatementAST
	

case class TableField( modifiers: List[FieldTypeModifier], typ: FieldType, name: String )


trait FieldType

case object StringType extends FieldType


trait FieldTypeModifier

case object UniqueModifier extends FieldTypeModifier

case object SecretModifier extends FieldTypeModifier


case class APIAST( table: Option[String], mappings: List[URIMapping] ) extends StatementAST
	
case class URIMapping( method: HTTPMethod, path: List[URISegment], action: ActionAST )

	
trait HTTPMethod

case object GETMethod extends HTTPMethod

case object POSTMethod extends HTTPMethod

case object PUTMethod extends HTTPMethod

case object DELETEMethod extends HTTPMethod

case object HEADMethod extends HTTPMethod

	
trait URISegment

case class NameURISegment( name: String ) extends URISegment

case class ParameterURISegment( name: String ) extends URISegment
	

case class ActionAST( name: String, args: List[String] )
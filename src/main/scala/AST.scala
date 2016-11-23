package xyz.hyperreal.informatio


trait AST


trait StatementAST extends AST

case class TableDefinition( name: String, fields: List[TableField] ) extends StatementAST
	
case class TableField( modifiers: List[FieldTypeModifier], typ: FieldType, name: String )


trait FieldType

case object StringType extends FieldType

case object UUIDType extends FieldType

case object DateType extends FieldType


trait FieldTypeModifier

case object UniqueModifier extends FieldTypeModifier

case object SecretModifier extends FieldTypeModifier


case class RoutesDefinition( base: URIPath, mappings: List[URIMapping] ) extends StatementAST
	
case class URIMapping( method: HTTPMethod, uri: URIPath, action: ExpressionAST )

case class URIPath( path: List[URISegment] )
	
	
trait HTTPMethod

case object GETMethod extends HTTPMethod

case object POSTMethod extends HTTPMethod

case object PUTMethod extends HTTPMethod

case object DELETEMethod extends HTTPMethod

case object HEADMethod extends HTTPMethod

	
trait URISegment

case class NameURISegment( name: String ) extends URISegment

case class ParameterURISegment( name: String ) extends URISegment
	

trait ExpressionAST extends AST

case class FunctionExpression( name: String, args: List[ExpressionAST] ) extends ExpressionAST

case class VariableExpression( name: String ) extends ExpressionAST
	
case class StringExpression( string: String ) extends ExpressionAST
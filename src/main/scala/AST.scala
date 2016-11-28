package xyz.hyperreal.cras


trait AST


trait StatementAST extends AST

case class TableDefinition( name: String, bases: List[URIPath], fields: List[TableColumn] ) extends StatementAST

case class TableColumn( modifiers: List[ColumnTypeModifier], typ: ColumnType, name: String )


trait ColumnType

case object StringType extends ColumnType

case object IntegerType extends ColumnType

case object UUIDType extends ColumnType

case object DateType extends ColumnType


trait ColumnTypeModifier

case object UniqueModifier extends ColumnTypeModifier

case object RequiredModifier extends ColumnTypeModifier

case object OptionalModifier extends ColumnTypeModifier

case object SecretModifier extends ColumnTypeModifier


case class RoutesDefinition( base: URIPath, mappings: List[URIMapping] ) extends StatementAST
	
case class URIMapping( method: HTTPMethod, uri: URIPath, action: ExpressionAST )

case class URIPath( path: List[URISegment] )

	
trait HTTPMethod

case object GETMethod extends HTTPMethod

case object POSTMethod extends HTTPMethod

case object PUTMethod extends HTTPMethod

case object PATCHMethod extends HTTPMethod

case object DELETEMethod extends HTTPMethod

	
trait URISegment

case class NameURISegment( segment: String ) extends URISegment

case class ParameterURISegment( name: String ) extends URISegment
	

trait ExpressionAST extends AST

case class FunctionExpression( name: String, args: List[ExpressionAST] ) extends ExpressionAST

case class VariableExpression( name: String ) extends ExpressionAST
	
case class StringExpression( string: String ) extends ExpressionAST
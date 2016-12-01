package xyz.hyperreal.cras

import util.parsing.input.Position


trait AST


trait StatementAST extends AST

case class TableDefinition( pos: Position, name: String, bases: List[URIPath], fields: List[TableColumn] ) extends StatementAST

case class TableColumn( pos: Position, modifiers: List[ColumnTypeModifier], typ: ColumnType, name: String )


trait ColumnType

case object StringType extends ColumnType

case object IntegerType extends ColumnType

case object UUIDType extends ColumnType

case object DateType extends ColumnType

case class ColumnTypeModifier( modifier: String, pos: Position )


case class RoutesDefinition( base: URIPath, mappings: List[URIMapping] ) extends StatementAST
	
case class URIMapping( method: HTTPMethod, uri: URIPath, action: ExpressionAST )

case class URIPath( path: List[URISegment] )

case class HTTPMethod( method: String )

	
trait URISegment

case class NameURISegment( segment: String ) extends URISegment

case class ParameterURISegment( name: String ) extends URISegment
	

trait ExpressionAST extends AST

case class FunctionExpression( name: String, args: List[ExpressionAST] ) extends ExpressionAST

case class VariableExpression( name: String ) extends ExpressionAST
	
case class LiteralExpression( value: Any ) extends ExpressionAST
package xyz.hyperreal.cras

import util.parsing.input.Position
import util.parsing.input.Positional

import xyz.hyperreal.lia.FunctionMap


trait AST


case class SourceAST( statements: List[StatementAST] ) extends AST


trait StatementAST extends AST

case class TableDefinition( pos: Position, name: String, bases: List[URIPath], fields: List[TableColumn] ) extends StatementAST

case class TableColumn( pos: Position, modifiers: List[ColumnTypeModifier], typ: ColumnType, name: String )


trait ColumnType

case object StringType extends ColumnType

case object IntegerType extends ColumnType

case object UUIDType extends ColumnType

case object DateType extends ColumnType

case class TableType( table: String ) extends ColumnType


case class ColumnTypeModifier( modifier: String, pos: Position )


case class RoutesDefinition( base: URIPath, mappings: List[URIMapping] ) extends StatementAST
	
case class URIMapping( method: HTTPMethod, uri: URIPath, action: ExpressionAST )

case class URIPath( path: List[URISegment] )

case class HTTPMethod( method: String )

	
trait URISegment

case class NameURISegment( segment: String ) extends URISegment

case class ParameterURISegment( name: String ) extends URISegment
	

trait ExpressionAST extends AST

case class ApplyExpression( function: ExpressionAST, args: List[ExpressionAST] ) extends ExpressionAST

case class VariableExpression( name: String ) extends ExpressionAST
	
case class LiteralExpression( value: Any ) extends ExpressionAST

case class ObjectExpression( pairs: List[(String, ExpressionAST)] ) extends ExpressionAST

//case class FunctionExpression( parts: List[FunctionPart] ) extends ExpressionAST
	
// case class FunctionPart( pattern: PatternAST, expr: ExpressionAST )
case class FunctionExpression( params: List[String], expr: ExpressionAST ) extends ExpressionAST

case class BinaryExpression( left: ExpressionAST, op: Symbol, func: FunctionMap, right: ExpressionAST ) extends ExpressionAST

case class UnaryExpression( op: Symbol, expr: ExpressionAST ) extends ExpressionAST

case class DotExpression( obj: ExpressionAST, prop: String ) extends ExpressionAST

case class CompoundExpression( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST

// trait PatternAST extends AST
// 
// case class VariablePattern( name: String ) extends PatternAST
// 
// case class StringPattern( s: String ) extends PatternAST
// 
// case class TuplePattern( components: List[PatternAST] ) extends PatternAST


// case class FunctionDefinition( pos: Position, name: String, function: FunctionPart ) extends StatementAST
case class FunctionDefinition( pos: Position, name: String, function: FunctionExpression ) extends StatementAST

case class VariableDefinition( name: String, value: ExpressionAST ) extends StatementAST with Positional

case class ValueDefinition( name: String, value: ExpressionAST ) extends StatementAST with Positional

case class ExpressionStatement( expr: ExpressionAST ) extends StatementAST
	
	
case class RouteActionAST( expr: ExpressionAST ) extends AST
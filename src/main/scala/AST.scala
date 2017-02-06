package xyz.hyperreal.energize

import util.parsing.input.Position
import util.parsing.input.Positional

import xyz.hyperreal.lia.FunctionMap


trait AST

case class SourceAST( statements: List[StatementAST] ) extends AST

trait StatementAST extends AST
case class TableDefinition( protection: Option[Option[String]], pos: Position, name: String, bases: List[URIPath],
														fields: List[TableColumn], resource: Boolean ) extends StatementAST

case class TableColumn( modifiers: List[ColumnTypeModifier], typ: ColumnType, name: String ) extends Positional

trait ReferenceType {
	val table: String
	var ref: Table
}

trait ColumnType extends Positional
trait PrimitiveColumnType extends ColumnType
case object StringType extends PrimitiveColumnType
case object IntegerType extends PrimitiveColumnType
case object LongType extends PrimitiveColumnType
case object UUIDType extends PrimitiveColumnType
case object DateType extends PrimitiveColumnType
case object DatetimeType extends PrimitiveColumnType
case object TimeType extends PrimitiveColumnType
case object TimestampType extends PrimitiveColumnType
case object TimestamptzType extends PrimitiveColumnType
case object BinaryType extends PrimitiveColumnType
case object FloatType extends PrimitiveColumnType
case class DecimalType( prec: Int, scale: Int ) extends PrimitiveColumnType
case class ArrayType( parm: PrimitiveColumnType, dpos: Position, dim: String, var dimint: Int ) extends ColumnType
case class SingleReferenceType( table: String, var ref: Table ) extends ColumnType with ReferenceType
case class ManyReferenceType( table: String, var ref: Table ) extends ColumnType with ReferenceType

case class ColumnTypeModifier( modifier: String ) extends Positional

case class RealmDefinition( pos: Position, realm: String ) extends StatementAST

case class RoutesDefinition( base: URIPath, mappings: List[URIMapping] ) extends StatementAST
	
case class URIMapping( method: HTTPMethod, uri: URIPath, action: ExpressionAST )

case class URIPath( path: List[URISegment] )

case class HTTPMethod( method: String )

trait URISegment
case class NameURISegment( segment: String ) extends URISegment
case class ParameterURISegment( name: String, typ: String ) extends URISegment

trait ExpressionAST extends AST
case class ApplyExpression( function: ExpressionAST, pos: Position, args: List[ExpressionAST] ) extends ExpressionAST
case class VariableExpression( name: String ) extends ExpressionAST
case class OptVariableExpression( name: String ) extends ExpressionAST
case class LiteralExpression( value: Any ) extends ExpressionAST
case class ObjectExpression( pairs: List[(String, ExpressionAST)] ) extends ExpressionAST
case class ListExpression( exprs: List[ExpressionAST] ) extends ExpressionAST
case class TupleExpression( first: ExpressionAST, rest: List[ExpressionAST] ) extends ExpressionAST
//case class FunctionExpression( parts: List[FunctionPart] ) extends ExpressionAST
// case class FunctionPart( pattern: PatternAST, expr: ExpressionAST )
case class FunctionExpression( params: List[String], expr: ExpressionAST ) extends ExpressionAST
case class BinaryExpression( left: ExpressionAST, op: Symbol, func: FunctionMap, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpression( op: Symbol, expr: ExpressionAST ) extends ExpressionAST
case class DotExpression( obj: ExpressionAST, prop: String ) extends ExpressionAST
case class CompoundExpression( left: ExpressionAST, right: ExpressionAST ) extends ExpressionAST
case class BlockExpression( l: List[StatementAST] ) extends ExpressionAST
case class ConditionalExpression( cond: List[(ExpressionAST, ExpressionAST)], no: Option[ExpressionAST] ) extends ExpressionAST
case class ForExpression( gen: List[GeneratorAST], body: ExpressionAST, e: Option[ExpressionAST] ) extends ExpressionAST
case class WhileExpression( cond: ExpressionAST, body: ExpressionAST, e: Option[ExpressionAST] ) extends ExpressionAST
case class ComparisonExpression( left: ExpressionAST, comps: List[(Symbol, FunctionMap, ExpressionAST)] ) extends ExpressionAST
case object BreakExpression extends ExpressionAST
case object ContinueExpression extends ExpressionAST
case class RangeExpression( start: ExpressionAST, end: ExpressionAST ) extends ExpressionAST
case class AssignmentExpression( v: String, expr: ExpressionAST ) extends ExpressionAST with Positional

case class GeneratorAST( pattern: String, traversable: ExpressionAST, filter: Option[ExpressionAST] ) extends AST

// trait PatternAST extends AST
// 
// case class VariablePattern( name: String ) extends PatternAST
// 
// case class StringPattern( s: String ) extends PatternAST
// 
// case class TuplePattern( components: List[PatternAST] ) extends PatternAST
	
// case class FunctionDefinition( pos: Position, name: String, function: FunctionPart ) extends StatementAST
case class FunctionDefinition( name: String, function: FunctionExpression ) extends StatementAST with Positional

case class VariableDefinition( name: String, value: ExpressionAST ) extends StatementAST with Positional

case class ValueDefinition( name: String, value: ExpressionAST ) extends StatementAST with Positional

case class ExpressionStatement( expr: ExpressionAST ) extends StatementAST
	
	
case class RouteActionAST( expr: ExpressionAST ) extends AST
package xyz.hyperreal.energize2

import xyz.hyperreal.bvm.{DeclarationStatementAST, ExpressionAST, VarAST}

import scala.util.parsing.input.Position


case class EnumDefinition( pos: Position, name: String, enum: List[(Position, String)] ) extends DeclarationStatementAST
case class RealmDefinition( pos: Position, realm: String ) extends DeclarationStatementAST
//case class BaseDeclaration( path: PathSegment ) extends DeclarationStatementAST
case class RoutesDefinition( pos: Position, base: Option[PathSegment], protection: Option[Option[String]],
														 mappings: Seq[RouteMapping] ) extends DeclarationStatementAST

trait PathSegment
case class LiteralPathSegment( s: String ) extends PathSegment
case class GroupedPathSegment( s: String ) extends PathSegment
case class TypePathSegment( typ: String ) extends PathSegment
case class ParameterPathSegment( pos: Position, param: String, seg: Option[PathSegment] ) extends PathSegment
case class AlternationPathSegment( segments: List[PathSegment] ) extends PathSegment
case class ConcatenationPathSegment( segments: List[PathSegment] ) extends PathSegment
case object SeqPathSegment extends PathSegment
case object SlashPathSegment extends PathSegment
case object ZeroOrMorePathSegment extends PathSegment
case object EmptyPathSegment extends PathSegment
case class OptionalPathSegment( seg: PathSegment ) extends PathSegment
case class OneOrMorePathSegment( seg: PathSegment ) extends PathSegment
case class RepeatPathSegment( subpat: PathSegment, lower: Int, pos: Position, upper: Option[Int] ) extends PathSegment

//trait ResourceField
//case class ArrayResourceField( pos: Position, name: String, typ: FieldTypeAST ) extends ResourceField
//case class SimpleResourceField( pos: Position, name: String, typ: FieldTypeAST ) extends ResourceField
//
//case class FieldTypeAST( pos: Position, name: String, args: Seq[Any] )

case class RouteMapping( pos: Position, method: String, path: PathSegment, guard: Option[ExpressionAST], action: ExpressionAST )

case class ResourceDefinition( protection: Option[Option[String]], pos: Position, name: String, base: Option[PathSegment],
														fields: List[ResourceField], resource: Boolean, var decl: VarAST = null ) extends DeclarationStatementAST

case class ResourceField( pos: Position, name: String, tpos: Position, tname: String, args: List[Any],
													modifiers: List[(Position, String)], array: Boolean )

/*
trait ColumnType extends Positional
trait PrimitiveColumnType extends ColumnType
case object BooleanType extends PrimitiveColumnType
case object StringType extends PrimitiveColumnType
case object TextType extends PrimitiveColumnType
case object IntegerType extends PrimitiveColumnType
case object LongType extends PrimitiveColumnType
case object UUIDType extends PrimitiveColumnType
case object DateType extends PrimitiveColumnType
case object DatetimeType extends PrimitiveColumnType
case object TimeType extends PrimitiveColumnType
case object TimestampType extends PrimitiveColumnType
case object TimestamptzType extends PrimitiveColumnType
case object BinaryType extends PrimitiveColumnType
case class BLOBType( rep: Symbol ) extends PrimitiveColumnType
case object FloatType extends PrimitiveColumnType
case class DecimalType( prec: Int, scale: Int ) extends PrimitiveColumnType
case class MediaType( allowed: List[MimeType], limit0: String, var limit: Int ) extends PrimitiveColumnType
case class ArrayType( parm: PrimitiveColumnType, dpos: Position, dim: String, var dimint: Int ) extends ColumnType
case class SingleReferenceType( table: String, var ref: Table ) extends ColumnType with ReferenceType
case class ManyReferenceType( table: String, var ref: Table ) extends ColumnType with ReferenceType
case class EnumType( name: String, enum: Vector[String] ) extends PrimitiveColumnType
case class IdentType( ident: String ) extends ColumnType

case class MimeType( typ: String, subtype: String )
*/


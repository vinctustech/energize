package xyz.hyperreal.energize

import xyz.hyperreal.bvm.{DeclarationStatementAST, ExpressionAST, VarAST}

import scala.util.parsing.input.Position


case class EnumDefinition( pos: Position, name: String, enum: List[(Position, String)] ) extends DeclarationStatementAST
case class RealmDefinition( pos: Position, realm: String ) extends DeclarationStatementAST
case class RoutesDefinition( pos: Position, base: Option[PathSegment], protection: Option[Option[String]],
														 mappings: Seq[RouteMapping] ) extends DeclarationStatementAST

trait PathSegment
case class LiteralPathSegment( s: String ) extends PathSegment
case class GroupedPathSegment( s: String ) extends PathSegment
case class TypePathSegment( typ: String ) extends PathSegment
case class ParameterPathSegment( pos: Position, param: String, seg: Option[PathSegment] ) extends PathSegment
case class AlternationPathSegment( segments: List[PathSegment] ) extends PathSegment
case class ConcatenationPathSegment( segments: List[PathSegment] ) extends PathSegment
case object RemainingPathSegment extends PathSegment
case object SlashPathSegment extends PathSegment
case object ZeroOrMorePathSegment extends PathSegment
case object EmptyPathSegment extends PathSegment
case class OptionalPathSegment( seg: PathSegment ) extends PathSegment
case class OneOrMorePathSegment( seg: PathSegment ) extends PathSegment
case class RepeatPathSegment( subpat: PathSegment, lower: Int, pos: Position, upper: Option[Int] ) extends PathSegment

case class RouteMapping( pos: Position, method: String, path: PathSegment, guard: Option[ExpressionAST], action: ExpressionAST )

case class ResourceDefinition( protection: Option[Option[String]], pos: Position, name: String, base: Option[PathSegment],
														fields: List[ResourceField], resource: Boolean, var decl: VarAST = null ) extends DeclarationStatementAST

case class ResourceField( pos: Position, name: String, tpos: Position, tname: String, args: List[Any],
													modifiers: List[(Position, String)], array: Boolean )

//@
package xyz.hyperreal.energize

import scala.util.parsing.input.Position

import xyz.hyperreal.bvm._
import xyz.hyperreal.funl2._


class EnergizeParser extends FunLParser {
	lexical.reserved += ("base", "enum", "table", "resource", "routes", "realm", "protected", "private")
	lexical.delimiters += ("=>", "?")

	import lexical.{Dedent, Indent, Newline}

	override lazy val declaration = super.declarationdef | enumDefinitions  | resourceDefinition | routesDefinition

	lazy val enumDefinitions =
		"enum" ~> enumDefinition |
		"enum" ~> Indent ~> rep1(enumDefinition <~ Newline) <~ Dedent ^^ DeclarationBlockAST

	lazy val enumDefinition: PackratParser[EnumDefinition] =
		pos ~ ident ~ ("[" ~> rep1sep(pos ~ ident, ",") <~ "]") ^^ {
			case p ~ i ~ e => EnumDefinition( p, i, e map {case ep ~ ei => (ep, ei)} ) }

	lazy val basicPath: PackratParser[PathSegment] =
		"/" ~> basicSlashPathSegment ^^ { p => ConcatenationPathSegment( List(SlashPathSegment, p) ) }

	lazy val basicSlashPathSegment: PackratParser[PathSegment] =
		basicSlashPathSegment ~ ("/" ~> basicPrimaryPathSegment) ^^ { case l ~ r => slash( l, r ) } |
		basicPrimaryPathSegment

	lazy val basicPrimaryPathSegment: PackratParser[PathSegment] =
		basicLiteralPathSegment |
		stringPathSegment

	lazy val realmDefinition: PackratParser[RealmDefinition] =
		"realm" ~> pos ~ stringLit ^^ { case p ~ r => RealmDefinition( p, r ) }

	lazy val resourceDefinition: PackratParser[ResourceDefinition] =
		("table" | "resource") ~ pos ~ ident ~ opt(protection) ~ opt(basicPath) ~ (Indent ~> rep1(tableColumn) <~ Dedent) ^^ {
			case k ~ p ~ name ~ pro ~ base ~ columns => ResourceDefinition( pro, p, name, base, columns, k == "resource" ) }

	lazy val protection: PackratParser[Option[String]] =
		"protected" ~> opt("(" ~> ident <~ ")") |
		"private" ^^^ Some( null )

	lazy val fieldModifiers: PackratParser[List[(Position, String)]] =
		rep(pos ~ ident) ^^ (_ map {case mp ~ mn => (mp, mn)})

	lazy val tableColumn: PackratParser[ResourceField] =
		pos ~ (ident|stringLit) ~ columnType ~ fieldModifiers /*~ opt(Indent ~> rep1(expression) <~ Dedent)*/ <~ nl ^^ {
			case p ~ name ~ ((tpos, tname, args)) ~ modifiers /*~ validators*/ =>
				ResourceField( p, name, tpos, tname, args, modifiers , false ) } |
		pos ~ (ident|stringLit) ~ ("[" ~> columnType <~ "]") ~ fieldModifiers /*~ opt(Indent ~> rep1(expression) <~ Dedent)*/ <~ nl ^^ {
			case p ~ name ~ ((tpos, tname, args)) ~ modifiers /*~ validators*/ =>
				ResourceField( p, name, tpos, tname, args, modifiers, true )
		}

	lazy val mimeType: PackratParser[MimeType] =
		(ident <~ "/") ~ (ident | "*") ^^ {case typ ~ subtype => MimeType( typ, subtype )}

	lazy val columnType: PackratParser[(Position, String, List[Any])] =
		pos ~ ident ~ ("(" ~> rep1sep(numericLit|ident|mimeType, ",") <~ ")") ^^ { case p ~ n ~ a => (p, n, a) } |
		pos ~ ident ^^ { case p ~ n => (p, n, Nil) }

	lazy val routesDefinition: PackratParser[RoutesDefinition] =
		"routes" ~> pos ~ opt(basicPath) ~ opt(protection) ~ (Indent ~> rep1(routeMapping) <~ Dedent) ^^ {
			case p ~ b ~ pro ~ ms => RoutesDefinition( p, b, pro, ms ) }

	lazy val routeMapping: PackratParser[RouteMapping] =
		pos ~ ident ~ opt(routePath) ~ opt("if" ~> expression) ~ ("=>" ~> expressionOrBlock <~ nl) ^^ {
			case p ~ m ~ Some( path ) ~ g ~ action => RouteMapping( p, m, path, g, action )
			case p ~ m ~ None ~ g ~ action => RouteMapping( p, m, EmptyPathSegment, g, action ) } |
		pos ~ ident ~ opt(routePath) ~ opt("if" ~> expression) ~ ("=>" ~> protection <~ nl) ^^ {
			case p ~ m ~ Some( path ) ~ None ~ pro => RouteMapping( p, m, path, Some(authorize(pro)), LiteralExpressionAST(null) )
			case p ~ m ~ None ~ None ~ pro => RouteMapping( p, m, EmptyPathSegment, Some(authorize(pro)), LiteralExpressionAST(null) )
			case p ~ m ~ Some( path ) ~ Some(g) ~ pro => RouteMapping( p, m, path, Some(AndExpressionAST(g, authorize(pro))), LiteralExpressionAST(null) )
			case p ~ m ~ None ~ Some(g) ~ pro => RouteMapping( p, m, EmptyPathSegment, Some(AndExpressionAST(g, authorize(pro))), LiteralExpressionAST(null) ) }

	def authorize( group: Option[String] ) =
		if (group contains null)
			BlockExpressionAST(List(
				ApplyExpressionAST(null, VariableExpressionAST(null, "access", "access"), null,
					List((null, DefinedOptionExpressionAST(DotExpressionAST(null, DotExpressionAST(null, VariableExpressionAST(null, "req", "req"), null, Symbol("query")), null, Symbol("access_token"))))), false),
				ApplyExpressionAST(null, FailExpressionAST, null, Nil, false)))
		else
			BlockExpressionAST(List(
				ApplyExpressionAST(null, VariableExpressionAST(null, "authorize", "authorize"), null,
					List(
						(null, LiteralExpressionAST(group)),
						(null, VariableExpressionAST(null, "req", "req"))), false),
				ApplyExpressionAST(null, FailExpressionAST, null, Nil, false)))

	lazy val routePath: PackratParser[PathSegment] =
		"/" ~> pathPathSegment ^^ { p => ConcatenationPathSegment( List(SlashPathSegment, p) ) } |
		"/" ^^^ SlashPathSegment

	lazy val pathPathSegment: PackratParser[PathSegment] = slashPathSegment

	def slash( left: PathSegment, right: PathSegment ) =
		left match {
			case ConcatenationPathSegment( segments ) => ConcatenationPathSegment( segments ++ List(SlashPathSegment, right) )
			case _ => ConcatenationPathSegment( List(left, SlashPathSegment, right) )
		}

	lazy val slashPathSegment: PackratParser[PathSegment] =
		slashPathSegment ~ ("/" ~> alternatePathSegment) ^^ { case l ~ r => slash( l, r ) } |
		alternatePathSegment

	lazy val alternatePathSegment: PackratParser[PathSegment] =
		opt(concatenatePathSegment) ~ rep("|" ~>  opt(concatenatePathSegment)) ^^ {
			case None ~ Nil => EmptyPathSegment
			case Some( p ) ~ Nil => p
			case Some( l ) ~ r => AlternationPathSegment( l :: r.map(_.getOrElse( EmptyPathSegment )) )
			case None ~ r => AlternationPathSegment( EmptyPathSegment +: r.map(_.getOrElse( EmptyPathSegment )) ) } |
		concatenatePathSegment

	lazy val concatenatePathSegment: PackratParser[PathSegment] =
		parameterPathSegment ~ rep(parameterPathSegment) ^^ {
			case p ~ Nil => p
			case l ~ r => ConcatenationPathSegment( l :: r ) } |
		parameterPathSegment

	lazy val parameterPathSegment: PackratParser[PathSegment] =
		(pos ~ ident <~ ":") ~ opt(quantifierPathSegment) ^^ { case p ~ n ~ s => ParameterPathSegment( p, n, s ) } |
		(pos ~ stringLit <~ ":") ~ opt(quantifierPathSegment) ^^ { case p ~ n ~ s => ParameterPathSegment( p, n, s ) } |
		quantifierPathSegment

	def quantifier( r: PathSegment, q: PathSegment => PathSegment ) =
		r match {
			case LiteralPathSegment( s ) if s.length > 1 => ConcatenationPathSegment( List(LiteralPathSegment(s.init), q(LiteralPathSegment(s.last.toString))) )
			case _ => q( r )
		}

	lazy val quantifierPathSegment: PackratParser[PathSegment] =
		primaryPathSegment <~ "?" ^^ (r => quantifier( r, OptionalPathSegment )) |
		primaryPathSegment <~ "+" ^^ (r => quantifier( r, OneOrMorePathSegment )) |
		primaryPathSegment ~ ("{" ~> number) ~ (opt("," ~> opt(pos ~ number)) <~ "}") ^^ {
			case p ~ l ~ None => quantifier( p, RepeatPathSegment(_, l.intValue, null, Some(l.intValue)) )
			case p ~ l ~ Some( None ) => quantifier( p, RepeatPathSegment(_, l.intValue, null, None) )
			case p ~ l ~ Some( Some(pos ~ u) ) => quantifier( p, RepeatPathSegment(_, l.intValue, pos, Some(u.intValue)) ) } |
		primaryPathSegment

	lazy val primaryPathSegment: PackratParser[PathSegment] =
		("." | "-") ^^ { s => LiteralPathSegment( s ) } |
		"*" ^^^ ZeroOrMorePathSegment |
		literalPathSegment |
		stringPathSegment |
		seqPathSegment |
		groupPathSegment

	lazy val seqPathSegment: PackratParser[PathSegment] = "..." ^^^ SeqPathSegment

	lazy val groupPathSegment: PackratParser[PathSegment] =
		"(" ~> pathPathSegment <~ ")" ^^ {
			case LiteralPathSegment( s ) => GroupedPathSegment( s )
			case p => p
		}

	lazy val literalPathSegment: PackratParser[PathSegment] = ident ^^ {
		case "int32" => TypePathSegment( "int32" )
		case "int64" => TypePathSegment( "int64" )
		case "integer" => TypePathSegment( "integer" )
		case "hex" => TypePathSegment( "hex" )
		case "string" => TypePathSegment( "string" )
		case f => LiteralPathSegment( f ) }

	lazy val basicLiteralPathSegment: PackratParser[PathSegment] = ident ^^ LiteralPathSegment

	lazy val stringPathSegment: PackratParser[PathSegment] = stringLit ^^ { case f => LiteralPathSegment( f ) }
}

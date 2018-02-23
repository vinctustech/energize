//@
package xyz.hyperreal.energize2

import java.sql.Statement

import collection.mutable.{ArrayBuffer, HashMap, LinkedHashMap}
import scala.util.parsing.input.Position
import xyz.hyperreal.bvm._
import xyz.hyperreal.funl2.Predef


case class Route( method: String, path: PatternAST, cond: ExpressionAST, action: ExpressionAST )

class EnergizeCompiler extends Compiler( Predef.constants ++ Predef.natives ++ Builtins.map, Predef.sysvars, Predef.macros ) {

	val ROUTER = "_router_"
	var router: FunctionExpressionAST = _
	val routes = new ArrayBuffer[Route]
	val resources = new HashMap[String, Resource]
	val conds = new ArrayBuffer[(ExpressionAST, ExpressionAST)]
	var lastResource: Position = _
	var db: Database = _
	var statement: Statement = _
	var internal: Boolean = _
	var specialsDone: Boolean = _

	def path2string( path: PathSegment ): String =
		path match {
			case ConcatenationPathSegment( segments ) => segments map path2string mkString
			case LiteralPathSegment( s ) => s
			case SlashPathSegment => "/"
		}

	def path2pattern( path: PathSegment ): PatternAST =
		path match {
			case ConcatenationPathSegment( segments ) =>
				def combine( l: List[PathSegment] ): List[PathSegment] =
					l match {
						case Nil => Nil
						case LiteralPathSegment(s1) :: LiteralPathSegment(s2) :: t => combine( LiteralPathSegment(s1 + s2) :: t )
						case SlashPathSegment :: LiteralPathSegment(s) :: t => combine( LiteralPathSegment('/' + s) :: t )
						case LiteralPathSegment(s) :: SlashPathSegment :: t => combine( LiteralPathSegment(s + '/') :: t )
						case EmptyPathSegment :: t => combine( t )
						case h :: EmptyPathSegment :: t => combine( h :: t )
						case ConcatenationPathSegment( s ) :: t => combine( s ++ t )
						case h :: ConcatenationPathSegment( s ) :: t => combine( h :: s ++ t )
						case h :: t => h :: combine( t )
					}

				ConcatenationPattern( combine(segments) map path2pattern )
			case AlternationPathSegment( segments ) => AlternationPattern( segments map path2pattern )
			case TypePathSegment( "integer"|"int32"|"int64" ) => INTEGER
			case TypePathSegment( "hex" ) => HEXINTEGER
			case TypePathSegment( "uuid" ) => UUID
			case TypePathSegment( "string" ) => OneOrMorePattern( ClassPattern(_ != '/') )
			case LiteralPathSegment( s ) => LiteralPattern( s )
			case GroupedPathSegment( s ) => LiteralPattern( s )
			case SlashPathSegment => LiteralPattern( "/" )
			case ParameterPathSegment( _, param, None ) => CapturePattern( param, OneOrMorePattern(ClassPattern(_ != '/')), _.toString )
			case ParameterPathSegment( _, param, Some(p@TypePathSegment(typ)) ) =>
				val conv: CharSequence => Any =
					typ match {
						case "integer" =>
							cs =>
								val num = BigInt( cs.toString )

								if (num.isValidInt)
									num.intValue.asInstanceOf[Number]
								else
									num
						case "hex" =>
							cs =>
								val num = BigInt( cs.toString, 16 )

								if (num.isValidInt)
									num.intValue.asInstanceOf[Number]
								else
									num
						case "int32" => cs => Integer.parseInt( cs.toString )
						case "int64" => cs => java.lang.Long.parseLong( cs.toString )
						case "uuid" => cs => java.util.UUID.fromString( cs.toString )
						case "string" => _.toString
					}

				CapturePattern( param, path2pattern(p), conv )
			case ParameterPathSegment( _, param, Some(p) ) => CapturePattern( param, path2pattern(p), null )
			case ZeroOrMorePathSegment => ZeroOrMorePattern( ClassPattern(_ != '/') )
			case EmptyPathSegment => EmptyPattern
			case OptionalPathSegment( p ) => OptionalPattern( path2pattern(p) )
			case OneOrMorePathSegment( p ) => OneOrMorePattern( path2pattern(p) )
			case RepeatPathSegment( subpat, lower, pos, upper ) => RepeatPattern( path2pattern(subpat), lower, pos, upper )
		}

	override def sourceExplicitsExtension( explicits: AST => Unit ) = {
		if (router == null && !internal) {
			specials
			val els = LiteralExpressionAST( 'nomatch )

			router =
				FunctionExpressionAST(ROUTER, null,
					List(
						VariableStructureAST(null, "$method", "$method"),
						VariableStructureAST(null, "$path", "$path"),
						VariableStructureAST(null, "$query", "$query"),
						VariableStructureAST(null, "$body", "$body"),
						VariableStructureAST(null, "$req", "$req"),
						VariableStructureAST(null, "res", "res")),
					false, List(
						FunctionPartExpressionAST(None,
							BlockExpressionAST( List(
								ValAST( VariableStructureAST(null, "req", "req"), null, LiteralExpressionAST(null) ),
									ScanExpressionAST(null, VariableExpressionAST(null, "$path", "$path"),
										ConditionalExpressionAST(conds, Some(els)))
							))
						)), WhereClauseAST(Nil))

			explicits( DefAST(ROUTER, router) )
		}
	}

	override def sourceDeclsExtension( decls: AST => Unit ): Unit = {
		if (!internal) {
			decls( DefAST(ROUTER, router) )
		}
	}

	override def sourceEmitExtension( emit: AST => Unit ) {
		if (!internal) {
			emit( DefAST(ROUTER, router) )
		}
	}

	override def explicitsExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (RoutesDefinition( pos, base, protection, mappings ), explicits) =>
			mappings foreach {
				case RouteMapping( pos, method, path, guard, action ) =>
					if (method exists (c => !c.isLetter || !c.isUpper))
						problem( pos, "request method must be uppercase word" )

					val abspath = ConcatenationPathSegment( List(base getOrElse(EmptyPathSegment), path) )
					val pat = path2pattern( abspath )
					val req =
						AndExpressionAST(
							BinaryExpressionAST(null, VariableExpressionAST(null, "$method", "$method"), '==, null, null, LiteralExpressionAST(method)),
							SetValueExpressionAST( null, "req", "req",
								BinaryExpressionAST( null, VariableExpressionAST(null, "$req", "$req"), '+, null, null,
									MapExpressionAST( List(
										(LiteralExpressionAST( "params" ),
											PatternExpressionStringsAST( ConcatenationPattern(List(BeginningOfInputPattern, pat, EndOfInputPattern)) ))
									) )
								)
							)
						)
					val cond =
						(if (guard nonEmpty) AndExpressionAST( req, guard get ) else req,
						protection match {
							case None => action
							case Some( Some(null) ) =>
								BlockExpressionAST(List(ApplyExpressionAST(null, VariableExpressionAST(null, "access", "access"), null,
									List(
										(null, DotExpressionAST(null,
											DotExpressionAST(null, VariableExpressionAST(null, "req", "req"), null, 'query), null, 'access_token))
									), false), action) )
							case Some( pro ) =>
								BlockExpressionAST(List(ApplyExpressionAST(null, VariableExpressionAST(null, "authorize", "authorize"), null,
									List(
										(null, LiteralExpressionAST(pro)),
										(null, DotExpressionAST(null,
											DotExpressionAST(null, VariableExpressionAST(null, "req", "req"), null, 'query), null, 'access_token))
									), false), action) )
						})

					conds += cond
					routes += Route( method, pat, cond._1, cond._2 )
			}
		case (r@ResourceDefinition( protection, pos, name, base, fields, resource, _ ), explicits) =>
			var mtm = false
			var ma = false

			if (resources.contains( db.desensitize(name) ))
				problem( pos, s"'$name' already defined" )

			val cols = new LinkedHashMap[String, Field]

			fields foreach {
				case col@ResourceField( pos, cname, tpos, tname, args, modifiers, array ) =>
					if (cols contains db.desensitize( cname ))
						problem( col.pos, s"field '$cname' defined twice" )

					val fieldType = Definition.dbtype( tpos, tname, args, array )

					var secret = false
					var required = false
					var optional = false
					var unique = false
					var indexed = false

					modifiers foreach {
						case (mp, "indexed") =>
							if (indexed)
								problem( mp, "modifier 'indexed' encountered more than once" )

							indexed = true
						case (mp, "unique") =>
							if (unique)
								problem( mp, "modifier 'unique' encountered more than once" )

							unique = true
						case (mp, "required") =>
							if (required)
								problem( mp, "modifier 'required' encountered more than once" )

							if (optional)
								problem( mp, "modifier 'required' encountered along with 'optional'" )

							required = true
						case (mp, "optional") =>
							if (optional)
								problem( mp, "modifier 'optional' encountered more than once" )

							if (required)
								problem( mp, "modifier 'optional' encountered along with 'required'" )

							optional = true
						case (mp, "secret" ) =>
							if (secret)
								problem( mp, "modifier 'secret' encountered more than once" )

							secret = true
						case (mp, m) =>
							problem( mp, s"unknown modifier '$m'" )
					}

					if (fieldType.isInstanceOf[ManyReferenceType])
						mtm = true

					if (fieldType.isInstanceOf[MediaType] && array)
						ma = true

					cols(cname) = Field( cname, fieldType, secret, required, unique, indexed, Nil )
			}

			val res = Resource( name, cols map {case (_, cinfo) => cinfo} toList, cols.toMap, resource, mtm, ma, statement, db )

			resources(db.desensitize( name )) = res
			r.decl = VarAST( pos, name, name, Some(LiteralExpressionAST(res)))
			explicits( r.decl )

			if (resource) {
				val mtm = cols.values exists (c => c.typ.isInstanceOf[ManyReferenceType])
				val authorize =
					protection match {
						case None => ""
						case Some( None ) => "protected"
						case Some( Some(null) ) => "private"
						case Some( Some(g) ) => s"protected ($g)"
					}

				if (base isEmpty) {
					addRoutes( Definition.compile( Builtins.routes("", name, authorize), db, statement, true ) )

					if (mtm)
						addRoutes( Definition.compile( Builtins.mtmroutes("", name, authorize), db, statement, true ) )

					if (cols exists {case (_, c) => c.typ.isInstanceOf[ArrayType]})
						addRoutes( Definition.compile( Builtins.arrayroutes("", name, authorize), db, statement, true ) )
				} else {
					val basepath = path2string( base get )

					addRoutes( Definition.compile( Builtins.routes(basepath, name, authorize), db, statement, true ) )

					if (mtm)
						addRoutes( Definition.compile( Builtins.mtmroutes(basepath, name, authorize), db, statement, true ) )

					if (cols exists { case (_, c) => c.typ.isInstanceOf[ArrayType] })
						addRoutes( Definition.compile( Builtins.arrayroutes(basepath, name, authorize), db, statement, true ) )
					}
			}

			lastResource = pos
	}

	private def addRoutes( d: Definition ): Unit = {
		conds ++= d.conds
		routes ++= d.routes
	}

	private def specials: Unit = {
		if (!internal && !specialsDone) {
			val special = Definition.compile( Builtins.special.
				replaceAll( "<base>", AUTHORIZATION.getString("base") ).
				replaceAll( "<email>", ADMIN.getString("email") ).
				replaceAll( "<password>", ADMIN.getString("password") ), db, statement, true )

			addRoutes( special )

			for ((k, v) <- special.resources) {
				resources get k match {
					case Some( Resource(name, fields, fieldMap, _, mtm, ma, st, d )) =>
						resources(k) = Resource(name, v.fields ++ fields, v.fieldMap ++ fieldMap, v.visible, v.manyToMany || mtm, ma, st, d )
					case None => resources(k) = v
				}
			}

			specialsDone = true
		}
	}

	override def declsExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (RoutesDefinition( pos, base, pro, mappings ), decls) =>
		case (ResourceDefinition( protection, pos, name, base, fields, resource, decl ), decls) => decls( decl )
	}

	override def emitExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (RoutesDefinition( pos, base, protection, mappings ), emit) =>
		case (ResourceDefinition( protection, pos, name, base, fields, resource, decl ), emit) => emit( decl )
	}

	def compile( ast: AST, db: Database, statement: Statement, internal: Boolean ) = {
		router = null
		conds.clear
		routes.clear
		this.db = db
		this.statement = statement
		this.internal = internal
		specialsDone = false
		super.compile( ast )
	}

}
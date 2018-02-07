//@
package xyz.hyperreal.energize2

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
			val els = LiteralExpressionAST( "no match" )

			router =
				FunctionExpressionAST(ROUTER, null,
					List(
						VariableStructureAST(null, "method", "method"),
						VariableStructureAST(null, "path", "path"),
						VariableStructureAST(null, "query", "query"),
						VariableStructureAST(null, "parms", "parms"),
						VariableStructureAST(null, "body", "body")),
					false, List(
						FunctionPartExpressionAST(None,
							BlockExpressionAST( List(
								VarAST( null, "req", "req", None ),
								ScanExpressionAST(null, VariableExpressionAST(null, "path", "path"),
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
						AssignmentExpressionAST( List((null, VariableExpressionAST(null, "req", "req"))), null, null, List((null,
							MapExpressionAST( List(
								(LiteralExpressionAST("method"),
									BinaryExpressionAST(null, VariableExpressionAST(null, "method", "method"), '==, null, null, LiteralExpressionAST(method))),
								(LiteralExpressionAST("path"),
									PatternExpressionStringsAST(ConcatenationPattern(List(BeginningOfInputPattern, pat, EndOfInputPattern)))),
								(LiteralExpressionAST("query"), VariableExpressionAST(null, "query", "query")),
								(LiteralExpressionAST("parms"), VariableExpressionAST(null, "parms", "parms")),
								(LiteralExpressionAST("body"), VariableExpressionAST(null, "body", "body"))
							))) )
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

					val fieldType =
						tname match {
							case "boolean" if args nonEmpty => problem( tpos, "the 'boolean' type doesn't take parameters" )
							case "boolean" => BooleanType
							case "char" if args.isEmpty || args.length > 1 || !integer( args.head.toString ) =>
								problem( tpos, "the 'char' type takes a positve integer parameter" )
							case "char" => CharType( args.head.toString.toInt )
							case "string" if args.length > 1 || args.nonEmpty && !integer( args.head.toString ) =>
								problem( tpos, "the 'string' type takes a positve integer size parameter, or else the size defaults to 128" )
							case "string" => StringType( if (args nonEmpty) args.head.toString.toInt else 128 )
							case "tinytext" if args nonEmpty => problem( tpos, "the 'tinytext' type doesn't take parameters" )
							case "tinytext" => TinytextType
							case "shorttext" if args nonEmpty => problem( tpos, "the 'shorttext' type doesn't take parameters" )
							case "shorttext" => ShorttextType
							case "text" if args nonEmpty => problem( tpos, "the 'text' type doesn't take parameters" )
							case "text" => TextType
							case "longtext" if args nonEmpty => problem( tpos, "the 'longtext' type doesn't take parameters" )
							case "longtext" => LongtextType
							case "tinyint" if args nonEmpty => problem( tpos, "the 'tinyint' type doesn't take parameters" )
							case "tinyint" => TinyintType
							case "smallint" if args nonEmpty => problem( tpos, "the 'smallint' type doesn't take parameters" )
							case "smallint" => SmallintType
							case "integer"|"int" if args nonEmpty => problem( tpos, "the 'integer' type doesn't take parameters" )
							case "integer"|"int" => IntegerType
							case "bigint" if args nonEmpty => problem( tpos, "the 'bigint' type doesn't take parameters" )
							case "bigint" => BigintType
							case "UUID"|"uuid" if args nonEmpty => problem( tpos, "the 'UUID' type doesn't take parameters" )
							case "UUID"|"uuid" => UUIDType
							case "date" if args nonEmpty => problem( tpos, "the 'date' type doesn't take parameters" )
							case "date" => DateType
							case "datetime" if args nonEmpty => problem( tpos, "the 'datetime' type doesn't take parameters" )
							case "datetime" => DatetimeType
							case "time" if args nonEmpty => problem( tpos, "the 'time' type doesn't take parameters" )
							case "time" => TimeType
							case "timestamp" if args nonEmpty => problem( tpos, "the 'timestamp' type doesn't take parameters" )
							case "timestamp" => TimestampType
							case "binary" if args nonEmpty => problem( tpos, "the 'binary' type doesn't take parameters" )
							case "binary" => BinaryType
							case "float" if args nonEmpty => problem( tpos, "the 'float' type doesn't take parameters" )
							case "float" => FloatType
							case "blob" if args.isEmpty || args.length > 1 || !(List("base64", "hex", "list", "urlchars") contains args.head.toString)  =>
								problem( tpos, "the 'blob' type takes a blob type parameter" )
							case "blob" => BLOBType( Symbol(args.head.toString) )
							case "media" if args exists (!_.isInstanceOf[MimeType]) =>
								problem( tpos, "the 'media' type takes zero or more media (MIME) type parameters" )
							case "media" => MediaType( args.asInstanceOf[List[MimeType]] )
							case r =>	//todo: could be EnumType if previously defined as such
								if (array)
									ManyReferenceType( tpos, r )
								else
									SingleReferenceType( tpos, r )
						}

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

			val res = Resource( name, cols map {case (_, cinfo) => cinfo} toList, cols.toMap, resource, mtm, ma )

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
					addRoutes( Definition.compile( Builtins.routes.replaceAll("<base>", "").replaceAll("<resource>", name).
						replaceAll("<authorize>", authorize), db, true ) )

					if (mtm)
						addRoutes( Definition.compile( Builtins.mtmroutes.replaceAll("<base>", "").replaceAll("<resource>", name).
							replaceAll("<authorize>", authorize), db, true ) )

					if (cols exists {case (_, c) => c.typ.isInstanceOf[ArrayType]})
						addRoutes( Definition.compile( Builtins.arrayroutes.replaceAll("<base>", "").replaceAll("<resource>", name).
							replaceAll("<authorize>", authorize), db, true ) )
				} else {
					val basepath = path2string( base get )

					addRoutes( Definition.compile( Builtins.routes.replaceAll("<resource>", name).
						replaceAll("<base>", basepath ).replaceAll("<authorize>", authorize), db, true ) )

					if (mtm)
						addRoutes( Definition.compile( Builtins.mtmroutes.replaceAll("<resource>", name).
							replaceAll("<base>", basepath).replaceAll("<authorize>", authorize), db, true ) )

					if (cols exists { case (_, c) => c.typ.isInstanceOf[ArrayType] })
						addRoutes( Definition.compile( Builtins.arrayroutes.replaceAll("<resource>", name).
							replaceAll("<base>", basepath).replaceAll("<authorize>", authorize), db, true ) )
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
				replaceAll( "<password>", ADMIN.getString("password") ), db, true )

			addRoutes( special )

			for ((k, v) <- special.resources) {
				resources get k match {
					case Some( Resource(name, fields, fieldMap, _, mtm, ma )) =>
						resources(k) = Resource(name, v.fields ++ fields, v.fieldMap ++ fieldMap, v.visible, v.manyToMany || mtm, ma )
					case None => resources(k) = v
				}
			}

			specialsDone = true
		}
	}

	override def declsExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (RoutesDefinition( pos, base, pro, mappings ), decls) =>
		case (ResourceDefinition( protection, pos, name, base, fields, resource, decl ), decls) =>
			decls( decl )
	}

	override def emitExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (RoutesDefinition( pos, base, protection, mappings ), emit) =>
		case (ResourceDefinition( protection, pos, name, base, fields, resource, decl ), emit) =>
			emit( decl )
	}

	def compile( ast: AST, db: Database, internal: Boolean ) = {
		router = null
		conds.clear
		routes.clear
		this.db = db
		this.internal = internal
		specialsDone = false
		super.compile( ast )
	}

}
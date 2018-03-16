//@
package xyz.hyperreal.energize

import java.sql.{Connection, Statement}

import collection.mutable.{ArrayBuffer, HashMap, LinkedHashMap}
import scala.util.parsing.input.Position
import xyz.hyperreal.bvm._
import xyz.hyperreal.funl2.Predef


case class Route( method: String, path: PathSegment, pat: PatternAST, cond: ExpressionAST, action: ExpressionAST )

class EnergizeCompiler extends Compiler( Predef.constants ++ Predef.natives ++ Builtins.map, Predef.sysvars, Predef.macros ) {

	val ROUTER = "_router_"
	var router: FunctionExpressionAST = _
	val routes = new ArrayBuffer[Route]
	val resources = new HashMap[String, Resource]
	val conds = new ArrayBuffer[(ExpressionAST, ExpressionAST)]
	val enums = new HashMap[String, Vector[String]]
	var connection: Connection = _
	var db: Database = _
	var statement: Statement = _
	var internal: Boolean = _

	def dbtype( pos: Position, name: String, args: List[Any], array: Boolean ) = {
		val basic =
			name match {
				case "boolean" =>
					if (args nonEmpty)
						problem( pos, "the 'boolean' type doesn't take parameters" )

					BooleanType
				case "char" =>
					if (args.isEmpty || args.length > 1 || !integer( args.head.toString ))
						problem( pos, "the 'char' type takes a positve integer parameter" )

					CharType( args.head.toString.toInt )
				case "string" =>
					if (args.length > 1 || args.nonEmpty && !integer( args.head.toString ))
						problem( pos, "the 'string' type takes a positve integer size parameter, or else the size defaults to 128" )

					StringType( if (args nonEmpty) args.head.toString.toInt else 128 )
				case "tinytext" =>
					if (args nonEmpty)
						problem( pos, "the 'tinytext' type doesn't take parameters" )

					TinytextType
				case "shorttext" =>
					if (args nonEmpty)
						problem( pos, "the 'shorttext' type doesn't take parameters" )

					ShorttextType
				case "text" =>
					if (args nonEmpty)
						problem( pos, "the 'text' type doesn't take parameters" )

					TextType
				case "longtext" =>
					if (args nonEmpty)
						problem( pos, "the 'longtext' type doesn't take parameters" )

					LongtextType
				case "tinyint" =>
					if (args nonEmpty)
						problem( pos, "the 'tinyint' type doesn't take parameters" )

					TinyintType
				case "smallint" =>
					if (args nonEmpty)
						problem( pos, "the 'smallint' type doesn't take parameters" )

					SmallintType
				case "integer"|"int" =>
					if (args nonEmpty)
						problem( pos, "the 'integer' type doesn't take parameters" )

					IntegerType
				case "bigint" =>
					if (args nonEmpty)
						problem( pos, "the 'bigint' type doesn't take parameters" )

					BigintType
				case "UUID"|"uuid" =>
					if (args nonEmpty)
						problem( pos, "the 'UUID' type doesn't take parameters" )

					UUIDType
				case "date" =>
					if (args nonEmpty)
						problem( pos, "the 'date' type doesn't take parameters" )

					DateType
				case "datetime" =>
					if (args nonEmpty)
						problem( pos, "the 'datetime' type doesn't take parameters" )

					DatetimeType
				case "time" =>
					if (args nonEmpty)
						problem( pos, "the 'time' type doesn't take parameters" )

					TimeType
				case "timestamp" =>
					if (args nonEmpty)
						problem( pos, "the 'timestamp' type doesn't take parameters" )

					TimestampType
				case "binary" =>
					if (args nonEmpty)
						problem( pos, "the 'binary' type doesn't take parameters" )

					BinaryType
				case "float" =>
					if (args nonEmpty)
						problem( pos, "the 'float' type doesn't take parameters" )

					FloatType
				case "blob" =>
					if (args.length > 1 || args.length == 1 && !(List("base64", "hex", "list", "urlchars") contains args.head.toString))
						problem( pos, """the 'blob' type takes an option blob type parameter: "base64", "hex", "list", or "urlchars"""" )

					if (args nonEmpty)
						BLOBType( Symbol(args.head.toString) )
					else
						BLOBType( 'base64 )
				case "media" =>
					if (args exists (!_.isInstanceOf[MimeType]))
						problem( pos, "the 'media' type takes zero or more media (MIME) type parameters" )

					MediaType( args.asInstanceOf[List[MimeType]] )
				case "decimal" =>
					if (args.length != 2 || args.nonEmpty && !integer( args.head.toString ) || args.length >= 2 && !integer( args.tail.head.toString ))
						problem( pos, "the 'decimal' type takes two integer parameters" )

					val (precision, scale) = (args.head.toString.toInt, args.tail.head.toString.toInt)

					if (precision < 1)
						problem( pos, "precision should be positive" )

					if (scale < 0)
						problem( pos, "scale should be non-negative" )

					if (scale > precision)
						problem( pos, "scale should be no greater than precision" )

					DecimalType( precision, scale )
				case _ if enums contains name => EnumType( name, enums(name) )
				case r =>
					if (array)
						ManyReferenceType( pos, r )
					else
						SingleReferenceType( pos, r )
			}

		if (array && !basic.isInstanceOf[ManyReferenceType])
			ArrayType( basic.asInstanceOf[PrimitiveFieldType] )
		else
			basic
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
		case (EnumDefinition( pos, name, enum ), explicits) =>
			enums(name) = enum.map( {case (_, e) => e} ).toVector
			explicits( DataAST(pos, name, enum map {case (p, e) => (e, Nil)}) )
		case (RoutesDefinition( _, base, protection, mappings ), _) =>
			mappings foreach {
				case RouteMapping( pos, method, path, guard, action ) =>
					if (method exists (c => !c.isLetter))
						problem( pos, "illegal request method" )

					val method1 = method.toUpperCase
					val abspath = ConcatenationPathSegment( List(base getOrElse EmptyPathSegment, path) )
					val pat = path2pattern( abspath )
					val req =
						AndExpressionAST(
							BinaryExpressionAST(null, VariableExpressionAST(null, "$method", "$method"), '==, null, null, LiteralExpressionAST(method1)),
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
										(null, DefinedOptionExpressionAST(DotExpressionAST(null, DotExpressionAST(null, VariableExpressionAST(null, "req", "req"), null, Symbol("query")), null, Symbol("access_token"))))
									), false), action) )
							case Some( pro ) =>
								BlockExpressionAST(List(ApplyExpressionAST(null, VariableExpressionAST(null, "authorize", "authorize"), null,
									List(
										(null, LiteralExpressionAST(pro)),
										(null, VariableExpressionAST(null, "req", "req"))
									), false), action) )
						})

					conds.insert( 0, cond )
					routes.insert( 0, Route( method1, abspath, pat, cond._1, cond._2 ) )
//					conds += cond
//					routes += Route( method1, abspath, pat, cond._1, cond._2 )
			}
		case (r@ResourceDefinition( protection, pos, name, base, fields, resource, _ ), explicits) =>
			var mtm = false
			var ma = false

			if (resources.contains( db.desensitize(name) ))
				problem( pos, s"'$name' already defined" )

			val cols = new LinkedHashMap[String, Field]

			fields foreach {
				case col@ResourceField( _, cname, tpos, tname, args, modifiers, array ) =>
					if (cols contains db.desensitize( cname ))
						problem( col.pos, s"field '$cname' defined twice" )

					val fieldType = dbtype( tpos, tname, args, array )

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

			val res = Resource( name, base, cols map {case (_, cinfo) => cinfo} toList, cols.toMap, resource, mtm, ma )

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
//					super.compile( Definition.parse( Builtins.routes("", name, authorize) ) )

					addRoutes( Definition.compile( Builtins.routes("", name, authorize), connection, db, statement, true ) )

					if (mtm)
						addRoutes( Definition.compile( Builtins.mtmroutes("", name, authorize), connection, db, statement, true ) )

					if (cols exists {case (_, c) => c.typ.isInstanceOf[ArrayType]})
						addRoutes( Definition.compile( Builtins.arrayroutes("", name, authorize), connection, db, statement, true ) )
				} else {
					val basepath = path2string( base get )

					addRoutes( Definition.compile( Builtins.routes(basepath, name, authorize), connection, db, statement, true ) )

					if (mtm)
						addRoutes( Definition.compile( Builtins.mtmroutes(basepath, name, authorize), connection, db, statement, true ) )

					if (cols exists { case (_, c) => c.typ.isInstanceOf[ArrayType] })
						addRoutes( Definition.compile( Builtins.arrayroutes(basepath, name, authorize), connection, db, statement, true ) )
					}
			}
	}

	private def addRoutes( d: Definition ): Unit = {
		conds.insertAll( 0, d.conds )
		routes.insertAll( 0, d.routes )
	}

	override def declsExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (_: RoutesDefinition, _) =>
		case (ResourceDefinition( _, _, _, _, _, _, decl ), decls) => decls( decl )
	}

	override def emitExtension: PartialFunction[(AST, AST => Unit), Any] = {
		case (_: EnumDefinition, _) =>
		case (_: RoutesDefinition, _) =>
		case (ResourceDefinition( _, _, _, _, _, _, decl ), emit) => emit( decl )
	}

	def compile( src: SourceAST, connection: Connection, db: Database, statement: Statement, internal: Boolean ) = {
		router = null
		conds.clear
		routes.clear
		this.connection = connection
		this.db = db
		this.statement = statement
		this.internal = internal

		if (!internal) {
			val spec = Definition.parse( Builtins.special(AUTHENTICATION.getString("base")) )
			val srcUsers =
				(src.statements find {
					case ResourceDefinition( _, _, "users", _, _, _, _ ) => true
					case _ => false
				}).asInstanceOf[Option[ResourceDefinition]]
			val srcWithoutUsers =
				src.statements filterNot {
					case ResourceDefinition( _, _, "users", _, _, _, _ ) => true
					case _ => false
				}
			val specUsers =
				(spec.statements find {
					case ResourceDefinition( _, _, "users", _, _, _, _ ) => true
					case _ => false
				}).asInstanceOf[Option[ResourceDefinition]]
			val ast =
				if (srcUsers.nonEmpty && specUsers.nonEmpty && srcUsers.get.base == specUsers.get.base) {
					val specWithNewUsers =
						spec.statements map {
							case ResourceDefinition( protection, pos, "users", base, fields, resource, _ ) =>
								ResourceDefinition( protection, pos, "users", base, fields ++ srcUsers.get.fields, resource )
							case s => s
						}

					SourceAST( specWithNewUsers ++ srcWithoutUsers )
				} else
					SourceAST( spec.statements ++ src.statements )

			super.compile( ast )
		}
		else
			super.compile( src )
	}

}
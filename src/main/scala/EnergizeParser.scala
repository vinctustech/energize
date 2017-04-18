package xyz.hyperreal.energize

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.CharArrayReader.EofCh
import util.parsing.input.{Positional, Reader, CharSequenceReader}

import xyz.hyperreal.indentation_lexical._
import xyz.hyperreal.lia.Math


class EnergizeParser extends StandardTokenParsers with PackratParsers
{
	override val lexical: IndentationLexical =
		new IndentationLexical( false, true, List("{", "[", "("), List("}", "]", ")"), "#", "/*", "*/" )
		{
			override def token: Parser[Token] = decimalParser | super.token

			override def identChar = letter | elem('_')// | elem('$')
			
			override def whitespace: Parser[Any] = rep[Any](
				whitespaceChar
				| '/' ~ '*' ~ comment
				| '#' ~ rep( chrExcept(EofCh, '\n') )
				| '/' ~ '*' ~ failure("unclosed comment")
				)
			
			private def decimalParser: Parser[Token] =
				rep1(digit) ~ optFraction ~ optExponent ^^
					{case intPart ~ frac ~ exp =>
						NumericLit( (intPart mkString "") :: frac :: exp :: Nil mkString)} //|
// 				fraction ~ optExponent ^^
// 					{case frac ~ exp => NumericLit( frac + exp )}

			private def chr( c: Char ) = elem( "", ch => ch == c )

			private def sign = chr( '+' ) | chr( '-' )

			private def optSign = opt( sign ) ^^
				{
					case None => ""
					case Some(sign) => sign
				}

			private def fraction = ('.' <~ not('.')) ~ rep( digit ) ^^
				{case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""}

			private def optFraction = opt( fraction ) ^^
				{
					case None => ""
					case Some( fraction ) => fraction
				}

			private def exponent = (chr( 'e' ) | chr( 'E' )) ~ optSign ~ rep1( digit ) ^^
				{case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""}

			private def optExponent = opt( exponent ) ^^
				{
					case None => ""
					case Some( exponent ) => exponent
				}

			reserved += (
				"if", "then", "else", "elif", "true", "false", "or", "and", "not", "null", "for", "while", "break", "continue",
				"def", "var", "val",
				"table", "resource", "unique", "indexed", "required", "optional", "secret", "routes",
				"string", "integer", "uuid", "date", "long", "array", "datetime", "time", "timestamp", "with", "timezone", "media",
				"blob", "binary",
				"GET", "POST", "PUT", "PATCH", "DELETE",
				"realm", "protected", "float", "decimal"
				)
			delimiters += (
				"+", "*", "-", "/", "\\", "//", "%", "^", "(", ")", "[", "]", "{", "}", ",", "=", "==", "/=", "<", ">", "<=", ">=",
				":", "->", ".", ";", "?", "<-", "..", "$"
				)
		}

	def parse[T]( grammar: PackratParser[T], r: Reader[Char] ) = phrase( grammar )( lexical.read(r) )
	
	def parseFromSource[T]( src: io.Source, grammar: PackratParser[T] ) = parseFromString( src.getLines.map(l => l + '\n').mkString, grammar )
	
	def parseFromString[T]( src: String, grammar: PackratParser[T] ) = {
		parse( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) => problem( rest.pos, error )
		}
	}

	import lexical.{Newline, Indent, Dedent}

	lazy val nl = rep1(Newline)

	lazy val onl = rep(Newline)

	lazy val source: PackratParser[SourceAST] =
		rep1(definitionStatement <~ nl | statements) ^^ (s => SourceAST( s.flatten )) |
		nl ^^^ SourceAST( Nil )

	lazy val statements: PackratParser[List[StatementAST]] =
		rep1(expressionStatement)
	
	lazy val expressionStatement: PackratParser[ExpressionStatement] = expression <~ nl ^^ ExpressionStatement
	
	lazy val definitionStatement: PackratParser[List[StatementAST]] =
		realmDefinition |
		tablesDefinition |
		routesDefinition |
		functionsDefinition |
		variablesDefinition |
		valuesDefinition

	lazy val realmDefinition: PackratParser[List[RealmDefinition]] =
		"realm" ~> pos ~ stringLit ^^ {case p ~ r => List( RealmDefinition(p.pos, r) )}

	lazy val functionsDefinition: PackratParser[List[FunctionDefinition]] =
		"def" ~> (Indent ~> rep1(functionDefinition <~ nl) <~ Dedent) |
		"def" ~> functionDefinition ^^ {f => List( f )}

	lazy val functionDefinition: PackratParser[FunctionDefinition] =
// 		pos ~ ident ~ parenparameters ~ ("=" ~> expression) ^^ {case p ~ n ~ pat ~ e => FunctionDefinition( p.pos, n, FunctionPart(pat, e) )}
		positioned( ident ~ parenparameters ~ ("=" ~> expression) ^^ {
			case n ~ pat ~ e => FunctionDefinition( n, FunctionExpression(pat, e) )} )
	
	lazy val variablesDefinition: PackratParser[List[VariableDefinition]] =
		"var" ~> variableDefinition ^^ {v => List( v )} |
		"var" ~> (Indent ~> rep1(variableDefinition <~ nl) <~ Dedent)
	
	lazy val valuesDefinition: PackratParser[List[ValueDefinition]] =
		"val" ~> valueDefinition ^^ {v => List( v )} |
		"val" ~> (Indent ~> rep1(valueDefinition <~ nl) <~ Dedent)
	
	lazy val variableDefinition: PackratParser[VariableDefinition] =
		positioned( ident ~ ("=" ~> expression) ^^ {case n ~ e => VariableDefinition( n, e )} )
	
	lazy val valueDefinition: PackratParser[ValueDefinition] =
		positioned( ident ~ ("=" ~> expression) ^^ {case n ~ e => ValueDefinition( n, e )} )
		
	lazy val pos = positioned( success(new Positional{}) )
	
	lazy val tablesDefinition: PackratParser[List[TableDefinition]] =
		("table" | "resource") ~ pos ~ ident ~ opt(protection) ~ opt(basePath) ~ (Indent ~> rep1(tableColumn) <~ Dedent) ^^ {
			case k ~ p ~ name ~ pro ~ base ~ columns => List( TableDefinition( pro, p.pos, name, base, columns, k == "resource" ) )}

	lazy val protection: PackratParser[Option[String]] =
		"protected" ~> opt("(" ~> ident <~ ")")

	lazy val tableColumn: PackratParser[TableColumn] =
		positioned( ident ~ columnType ~ rep(columnModifier) <~ nl ^^ {
			case name ~ typ ~ modifiers =>
				TableColumn( name, typ, modifiers )} )

	lazy val columnType: PackratParser[ColumnType] =
		positioned(
			primitiveColumnType ~ (("array" ~ "(") ~> pos) ~ (numericLit <~ ")") ^^ {case t ~ p ~ d => ArrayType( t, p.pos, d, -1 )} |
			primitiveColumnType <~ "array" ^^ (t => ArrayType( t, null, null, 1 )) |
			primitiveColumnType |
			ident <~ "array" ^^ (ManyReferenceType( _, null )) |
			ident ^^ (SingleReferenceType( _, null ))
		)

	lazy val mimeType: PackratParser[MimeType] =
		(ident <~ "/") ~ (ident | "*") ^^ {case typ ~ subtype => MimeType( typ, subtype )}

	lazy val primitiveColumnType: PackratParser[PrimitiveColumnType] =
		"string" ^^^ StringType |
		"integer" ^^^ IntegerType |
		"long" ^^^ LongType |
		"uuid" ^^^ UUIDType |
		"date" ^^^ DateType |
		"datetime" ^^^ DatetimeType |
		"time" ^^^ TimeType |
		"timestamp" ^^^ TimestampType |
		"timestamp" ~ "with" ~ "timezone" ^^^ TimestamptzType |
		"binary" ^^^ BinaryType |
		"blob" ~> opt("(" ~> ident <~ ")") ^^ {
			case None => BLOBType( 'base64 )
			case Some( r ) => BLOBType( Symbol(r) )} |
		"float" ^^^ FloatType |
		("decimal" ~ "(") ~> ((numericLit <~ ",") ~ (numericLit <~ ")")) ^^ {
			case p ~ s => DecimalType( p.toInt, s.toInt )} |
		"media" ~> opt("(" ~> (repsep(mimeType, ",") ~ opt("," ~> numericLit)) <~ ")") ^^ {
			case Some( ts ~ Some(l) ) => MediaType( ts, l, Int.MaxValue )
			case Some( ts ~ None ) => MediaType( ts, null, Int.MaxValue )
			case None => MediaType( Nil, null, Int.MaxValue )}

	lazy val columnModifier: PackratParser[ColumnTypeModifier] =
		positioned( ("unique" | "indexed" | "required" | "optional" | "secret") ^^ ColumnTypeModifier )
		
	lazy val routesDefinition: PackratParser[List[RoutesDefinition]] =
		"routes" ~> opt(basePath) ~ opt(protection) ~ (Indent ~> rep1(uriMapping) <~ Dedent) ^^ {
			case Some( base ) ~ pro ~ mappings =>
				List( RoutesDefinition(base, pro, mappings) )
			case None ~ pro ~ mappings =>
				List( RoutesDefinition(URIPath(Nil), pro, mappings) )
		}

	def authorize( group: Option[String] ) = CompoundExpression(ApplyExpression(VariableExpression("authorize"), null, List(LiteralExpression(group))), ApplyExpression(VariableExpression("reject"), null, Nil))

	lazy val uriMapping: PackratParser[URIMapping] =
		httpMethod ~ "/" ~ actionExpression <~ nl ^^ {case method ~ _ ~ action => URIMapping( method, URIPath(Nil), action )} |
		httpMethod ~ uriPath ~ actionExpression <~ nl ^^ {case method ~ uri ~ action => URIMapping( method, uri, action )} |
		httpMethod ~ "/" ~ protection <~ nl ^^ {case method ~ _ ~ group => URIMapping( method, URIPath(Nil), authorize(group) )} |
		httpMethod ~ uriPath ~ protection <~ nl ^^ {case method ~ uri ~ group => URIMapping( method, uri, authorize(group) )}

	lazy val httpMethod: PackratParser[HTTPMethod] =
		("GET" | "POST" | "PUT" | "PATCH" | "DELETE") ^^ HTTPMethod
		
	lazy val basePath: PackratParser[URIPath] =
		"/" ~> rep1sep(nameSegment, "/") ^^ URIPath
		
	lazy val uriPath: PackratParser[URIPath] =
		"/" ~> rep1sep(uriSegment, "/") ^^ URIPath
	
	lazy val uriSegment: PackratParser[URISegment] =
		ident ~ (":" ~> opt(segmentType)) ^^ {
			case n ~ Some( t ) => ParameterURISegment( n, t )
			case n ~ None => ParameterURISegment( n, "string" )} |
		nameSegment
	
	lazy val segmentType: PackratParser[String] =
		"string" |
		"integer" |
		"long"
		
	lazy val nameSegment: PackratParser[NameURISegment] =
		ident ^^ NameURISegment |
		stringLit ^^ NameURISegment
		
	lazy val mathSymbols = Set( '+, '-, '*, '/, '//, Symbol("\\"), Symbol("\\%"), '^, '%, 'mod, '|, '/|, '==, '!=, '<, '>, '<=, '>= )
	
	def lookup( s: Symbol ) =
		if (mathSymbols contains s)
			Math.lookup( s )
		else
			null
	
	lazy val expression: PackratParser[ExpressionAST] =
		compoundExpression
	
	lazy val compoundExpression: PackratParser[ExpressionAST] =
		(compoundExpression <~ ";") ~ controlExpression ^^ {case left ~ right => CompoundExpression( left, right )} |
		controlExpression
	
	lazy val controlExpression: PackratParser[ExpressionAST] =
		("if" ~> booleanExpression) ~ ("then" ~> expressionOrBlock | blockExpression) ~ rep(elif) ~ elsePart ^^
			{case c ~ t ~ ei ~ e => ConditionalExpression( (c, t) +: ei, e )} |
		"for" ~> generators ~ ("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^
			{case g ~ b ~ e => ForExpression( g, b, e )} |
		"while" ~> expression ~ ("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^
			{case c ~ b ~ e => WhileExpression( c, b, e )} |
		"break" ^^^ BreakExpression |
		"continue" ^^^ ContinueExpression |
		booleanExpression

	lazy val elif =
		(onl ~ "elif") ~> booleanExpression ~ ("then" ~> expressionOrBlock | blockExpression) ^^ {case c ~ t => (c, t)}

	lazy val elsePart: PackratParser[Option[ExpressionAST]] = opt(onl ~> "else" ~> expressionOrBlock)

	lazy val generator: PackratParser[GeneratorAST] =
		(ident <~ "<-") ~ expression ~ opt("if" ~> expression) ^^ {case p ~ t ~ f => GeneratorAST( p, t, f )}

	lazy val generators = rep1sep(generator, ",")
		
	lazy val booleanExpression =
		comparisonExpression

	lazy val comparisonExpression: PackratParser[ExpressionAST] =
		assignmentExpression ~ rep1(("==" | "!=" | "<" | ">" | "<=" | ">=") ~ assignmentExpression) ^^
			{case l ~ comps => ComparisonExpression( l, comps map {
				case o ~ e =>
					val s = Symbol( o )
					
					(s, Math.lookup(s), e)} )} |
		assignmentExpression
	
	lazy val assignmentExpression: PackratParser[ExpressionAST] =
		positioned( ident ~ ("=" ~> rangeExpression) ^^ {case v ~ e => AssignmentExpression( v, e )} ) |
		rangeExpression
	
	lazy val rangeExpression: PackratParser[ExpressionAST] =
		additiveExpression ~ (".." ~> additiveExpression) ^^ {case s ~ e => RangeExpression( s, e )} |
		additiveExpression
		
	lazy val additiveExpression: PackratParser[ExpressionAST] =
		additiveExpression ~ ("+" | "-") ~ multiplicativeExpression ^^
			{case l ~ o ~ r =>
				val s = Symbol(o)
				
				BinaryExpression( l, s, lookup(s), r )} |
		multiplicativeExpression
		
	lazy val multiplicativeExpression: PackratParser[ExpressionAST] =
		multiplicativeExpression ~ ("*" | "/" | "\\" | "%" | "//") ~ exponentialExpression ^^
			{case l ~ o ~ r =>
				val s = Symbol(o)
				
				BinaryExpression( l, s, lookup(s), r )} |
		multiplicativeExpression ~ applyExpression ^^
			{case l ~ r => BinaryExpression( l, '*, lookup('*), r )} |
		exponentialExpression

	lazy val exponentialExpression: PackratParser[ExpressionAST] =
		exponentialExpression ~ "^" ~ negationExpression ^^
			{case l ~ _ ~ r => BinaryExpression( l, '^, lookup('^), r )} |
		negationExpression

	lazy val negationExpression: PackratParser[ExpressionAST] =
		"-" ~> negationExpression ^^ {
			case LiteralExpression( n: Number ) => LiteralExpression( Math('-, n) )
			case v => UnaryExpression( '-, v )
			} |
		applyExpression
		
	lazy val applyExpression: PackratParser[ExpressionAST] =
		applyExpression ~ ("(" ~> pos) ~ (repsep(expression, ",") <~ ")") ^^ {case func ~ p ~ args => ApplyExpression( func, p.pos, args )} |
		applyExpression ~ ("." ~> ident) ^^ {case o ~ p => DotExpression( o, p )} |
		primaryExpression
		
	lazy val primaryExpression: PackratParser[ExpressionAST] =
		numericLit ^^
			{n =>
				if (n matches ".*(\\.|e|E).*")
					LiteralExpression( n.toDouble )
				else
					LiteralExpression( n.toInt )
			} |
		functionLiteral |
		variableExpression |
		queryParameterExpression |
		pathParameterExpression |
		systemValueExpression |
		stringLit ^^ LiteralExpression |
		("true"|"false") ^^ (b => LiteralExpression( b.toBoolean )) |
		"null" ^^^ LiteralExpression( null ) |
		("(" ~> expression <~ ",") ~ (rep1sep(expression, ",") <~ ")") ^^ {case f ~ r => TupleExpression( f, r )} |
		"(" ~> expression <~ ")" |
		"{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpression |
		"[" ~> repsep(expression, ",") <~ "]" ^^ ListExpression
		
	lazy val pair: PackratParser[(String, ExpressionAST)] = (ident|stringLit) ~ (":" ~> expression) ^^ {case k ~ v => (k, v)}
		
	lazy val variableExpression = ident ^^ {n => VariableExpression( n , None )}
	
	lazy val queryParameterExpression = "?" ~> ident ^^ QueryParameterExpression

	lazy val pathParameterExpression = "/" ~> ident ^^ PathParameterExpression

	lazy val systemValueExpression = "$" ~> ident ^^ (n => SystemValueExpression( n ))

	lazy val functionLiteral =
		parameters ~ ("->" ~> expression) ^^ {case p ~ e => FunctionExpression( p, e )}
		
// 	lazy val functionLiteral =
// 		functionPart ^^ (p => FunctionExpression( List(p) )) |
// 		Indent ~> rep1(functionPart <~ nl) <~ Dedent ^^ (FunctionExpression)
// 	
// 	lazy val functionPart =
// 		parameters ~ ("->" ~> expression) ^^ {case p ~ e => FunctionPart( p, e )}
		
	lazy val parameters =
		ident ^^ {p => List(p)} |
		parenparameters
		
	lazy val parenparameters = "(" ~> repsep(ident, ",") <~ ")"
		
// 	lazy val pattern: PackratParser[PatternAST] =
// 		ident ^^ (VariablePattern) |
// 		stringLit ^^ (StringPattern) |
// 		"(" ~> rep1sep( pattern, ",") <~ ")" ^^ (TuplePattern)
		
	lazy val blockExpression =
		Indent ~> statements <~ Dedent ^^ BlockExpression

	lazy val expressionOrBlock = expression | blockExpression
	
	lazy val actionExpression: PackratParser[ExpressionAST] =
		actionCompoundExpression
		
	lazy val actionCompoundExpression: PackratParser[ExpressionAST] =
		(actionCompoundExpression <~ ";") ~ actionApplyExpression ^^ {case left ~ right => CompoundExpression( left, right )} |
		actionApplyExpression
		
	lazy val actionApplyExpression: PackratParser[ExpressionAST] =
		actionApplyExpression ~ ("(" ~> pos) ~ (repsep(expression, ",") <~ ")") ^^ {case name ~ p ~ args => ApplyExpression( name, p.pos, args )} |
		actionPrimaryExpression
		
	lazy val actionPrimaryExpression: PackratParser[ExpressionAST] =
		variableExpression |
		queryParameterExpression |
		pathParameterExpression |
		systemValueExpression |
		"null" ^^^ LiteralExpression( null ) |
		"{" ~> repsep(pair, ",") <~ "}" ^^ ObjectExpression

}

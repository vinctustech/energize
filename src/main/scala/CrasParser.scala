package xyz.hyperreal.cras

import java.{lang => boxed}

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.CharArrayReader.EofCh
import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.{Positional, Reader, Position, CharSequenceReader}

import xyz.hyperreal.indentation_lexical._
import xyz.hyperreal.lia.Math


class CrasParser extends StandardTokenParsers with PackratParsers
{
	override val lexical: IndentationLexical =
		new IndentationLexical( false, true, List("{", "[", "("), List("}", "]", ")"), "//", "/*", "*/" )
		{
//			override def token: Parser[Token] = decimalParser | super.token

			override def identChar = letter | elem('_') | elem('$')
			
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
				"if", "then", "else", "elif", "true", "false", "or", "and", "not", "null",
				"resource", "unique", "required", "string", "optional", "integer", "secret", "route", "uuid", "date", "GET", "POST", "PUT", "PATCH", "DELETE",
				"def", "var"
				)
			delimiters += (
				"+", "*", "-", "/", "\\", "//", "%", "^", "(", ")", "[", "]", "{", "}", ",", "=", "==", "/=", "<", ">", "<=", ">=", ":", "->", "."
				)
		}

	def parse[T <: AST]( grammar: PackratParser[T], r: Reader[Char] ) = phrase( grammar )( lexical.read(r) )
	
	def parseFromSource[T <: AST]( src: io.Source, grammar: PackratParser[T] ) = parseFromString( src.getLines.map(l => l + '\n').mkString, grammar )
	
	def parseFromString[T <: AST]( src: String, grammar: PackratParser[T] ) = {
		parse( grammar, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => tree
			case NoSuccess( error, rest ) =>
				problem( rest.pos, error )
		}
	}

	import lexical.{Newline, Indent, Dedent}

	lazy val nl = rep1(Newline)

	lazy val source: PackratParser[SourceAST] =
		rep1(definitionStatement <~ nl | (expressionStatement ^^ (e => List( e )))) ^^ (s => SourceAST( s.flatten )) |
		nl ^^^ SourceAST( Nil )

	lazy val expressionStatement: PackratParser[ExpressionStatement] = expression <~ nl ^^ (ExpressionStatement)
	
	lazy val definitionStatement: PackratParser[List[StatementAST]] =
		tablesDefinition |
		routesDefinition |
		functionsDefinition |
		variablesDefinition
	
	lazy val functionsDefinition: PackratParser[List[FunctionDefinition]] =
		"def" ~> functionDefinition ^^ {f => List( f )} |
		"def" ~> (Indent ~> rep1(functionDefinition <~ nl) <~ Dedent)
		
	lazy val functionDefinition: PackratParser[FunctionDefinition] =
// 		pos ~ ident ~ parenparameters ~ ("=" ~> expression) ^^ {case p ~ n ~ pat ~ e => FunctionDefinition( p.pos, n, FunctionPart(pat, e) )}
		pos ~ ident ~ parenparameters ~ ("=" ~> expression) ^^ {case p ~ n ~ pat ~ e => FunctionDefinition( p.pos, n, FunctionExpression(pat, e) )}
	
	lazy val variablesDefinition: PackratParser[List[VariableDefinition]] =
		"var" ~> variableDefinition ^^ {v => List( v )} |
		"var" ~> (Indent ~> rep1(variableDefinition <~ nl) <~ Dedent)
	
	lazy val variableDefinition: PackratParser[VariableDefinition] =
		positioned( ident ~ ("=" ~> expression) ^^ {case n ~ e => VariableDefinition( n, e )} )
		
	lazy val pos = positioned( success(new Positional{}) )
	
	lazy val tablesDefinition: PackratParser[List[TableDefinition]] =
		"resource" ~> pos ~ ident ~ repsep(basePath, ",") ~ (Indent ~> rep1(tableColumn) <~ Dedent) ^^ {
			case p ~ name ~ bases ~ columns => List( TableDefinition( p.pos, name, bases, columns ) )}
		
	lazy val tableColumn: PackratParser[TableColumn] =
		pos ~ ident ~ columnType ~ rep(columnModifier) <~ nl ^^ {
			case p ~ name ~ typ ~ modifiers =>
				TableColumn( p.pos, modifiers, typ, name )}

	lazy val columnType: PackratParser[ColumnType] =
		"string" ^^^ StringType |
		"integer" ^^^ IntegerType |
		"uuid" ^^^ UUIDType |
		"date" ^^^ DateType |
		ident ^^ (TableType)
		
	lazy val columnModifier: PackratParser[ColumnTypeModifier] =
		pos ~ ("unique" | "required" | "optional" | "secret") ^^ {case p ~ m => ColumnTypeModifier( m, p.pos )}
		
	lazy val routesDefinition: PackratParser[List[RoutesDefinition]] =
		"route" ~> opt(basePath) ~ (Indent ~> rep1(uriMapping) <~ Dedent) ^^ {
			case Some( base ) ~ mappings => 
				List( RoutesDefinition( base, mappings ) )
			case None ~ mappings =>
				List( RoutesDefinition( URIPath(Nil), mappings ) )
		}
		
	lazy val uriMapping: PackratParser[URIMapping] =
		httpMethod ~ "/" ~ actionExpression <~ nl ^^ {case method ~ _ ~ action => URIMapping( method, URIPath(Nil), action )} |
		httpMethod ~ uriPath ~ actionExpression <~ nl ^^ {case method ~ uri ~ action => URIMapping( method, uri, action )}
		
	lazy val httpMethod: PackratParser[HTTPMethod] =
		("GET" | "POST" | "PUT" | "PATCH" | "DELETE") ^^ (HTTPMethod)
		
	lazy val basePath: PackratParser[URIPath] =
		"/" ~> rep1sep(nameSegment, "/") ^^ (URIPath)
		
	lazy val uriPath: PackratParser[URIPath] =
		"/" ~> rep1sep(uriSegment, "/") ^^ (URIPath)
	
	lazy val uriSegment: PackratParser[URISegment] =
		ident ^^ (NameURISegment) |
		":" ~> ident ^^ (ParameterURISegment)
	
	lazy val nameSegment: PackratParser[NameURISegment] =
		ident ^^ (NameURISegment)
		
	lazy val mathSymbols = Set( '+, '-, '*, '/, '//, Symbol("\\"), Symbol("\\%"), '^, '%, 'mod, '|, '/|, '==, '!=, '<, '>, '<=, '>= )
	
	def lookup( s: Symbol ) =
		if (mathSymbols contains s)
			Math.lookup( s )
		else
			null
	
	lazy val expression: PackratParser[ExpressionAST] =
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
		applyExpression ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {case name ~ args => ApplyExpression( name, args )} |
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
		ident ^^ (VariableExpression) |
		stringLit ^^ (LiteralExpression) |
		("true"|"false") ^^ (b => LiteralExpression( b.toBoolean )) |
		"null" ^^^ (LiteralExpression( null )) |
		"(" ~> expression <~ ")" |
		"{" ~> repsep(pair, ",") <~ "}" ^^ (ObjectExpression)
	
	lazy val pair: PackratParser[(String, ExpressionAST)] = (ident|stringLit) ~ (":" ~> expression) ^^ {case k ~ v => (k, v)}
		
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
	
	lazy val actionExpression: PackratParser[ExpressionAST] =
		actionApplyExpression
		
	lazy val actionApplyExpression: PackratParser[ExpressionAST] =
		actionApplyExpression ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {case name ~ args => ApplyExpression( name, args )} |
		actionPrimaryExpression
		
	lazy val actionPrimaryExpression: PackratParser[ExpressionAST] =
		ident ^^ (VariableExpression) |
		"{" ~> repsep(pair, ",") <~ "}" ^^ (ObjectExpression)

}

package xyz.hyperreal.cras

import java.{lang => boxed}

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.CharArrayReader.EofCh
import util.parsing.combinator.lexical.StdLexical
import util.parsing.input.{Positional, Reader}

import xyz.hyperreal.indentation_lexical._


class CrasParser extends StandardTokenParsers with PackratParsers
{
	override val lexical: IndentationLexical =
		new IndentationLexical( false, true, List("{", "[", "("), List("}", "]", ")"), "//", "/*", "*/" )
		{
			override def token: Parser[Token] = decimalParser | super.token

			override def identChar = letter | elem('_') | elem('$')
			
			override def whitespace: Parser[Any] = rep[Any](
				whitespaceChar
				| '/' ~ '*' ~ comment
				| '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
				| '/' ~ '*' ~ failure("unclosed comment")
				)
			
			private def decimalParser: Parser[Token] =
				rep1(digit) ~ optFraction ~ optExponent ^^
					{case intPart ~ frac ~ exp =>
						NumericLit( (intPart mkString "") :: frac :: exp :: Nil mkString)} |
				fraction ~ optExponent ^^
					{case frac ~ exp => NumericLit( frac + exp )}

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
				"table", "unique", "required", "string", "optional", "integer", "secret", "route", "uuid", "date", "GET", "POST", "PUT", "PATCH", "DELETE",
				"result", "def"
				)
			delimiters += (
				"+", "*", "-", "/", "^", "(", ")", "[", "]", "{", "}", ",", "=", "==", "/=", "<", ">", "<=", ">=", ":", "->"
				)
		}

	def parse( r: Reader[Char] ) = phrase( source )( lexical.read(r) )

	import lexical.{Newline, Indent, Dedent}

	lazy val onl = opt(Newline)

	lazy val source =
		rep1(statements <~ Newline) ^^ (s => SourceAST( s.flatten )) |
		Newline ^^^ SourceAST( Nil )

	lazy val statements: PackratParser[List[StatementAST]] =
		tablesDefinition |
		routesDefinition |
		resultsDefinition |
		functionsDefinition
	
	lazy val functionsDefinition =
		"def" ~> functionDefinition ^^ {f => List( f )}
		
	lazy val functionDefinition = ident ~ ("(" ~> pattern <~ ")") ~ ("=" ~> expression) ^^ {case n ~ p ~ e => FunctionDefinition( n, FunctionPart(p, e) )}
	
	lazy val pos = positioned( success(new Positional{}) )
	
	lazy val resultsDefinition =
		"result" ~> functionLiteral ^^ (r => List( ResultsDefinition(r) ))
	
	lazy val tablesDefinition: PackratParser[List[TableDefinition]] =
		"table" ~> pos ~ ident ~ repsep(uriPath, ",") ~ (Indent ~> rep1(tableColumn) <~ Dedent) ^^ {
			case p ~ name ~ bases ~ columns => List( TableDefinition( p.pos, name, bases, columns ) )}
		
	lazy val tableColumn: PackratParser[TableColumn] =
		pos ~ ident ~ columnType ~ rep(columnModifier) <~ Newline ^^ {case p ~ name ~ typ ~ modifiers => TableColumn( p.pos, modifiers, typ, name )}

	lazy val columnType: PackratParser[ColumnType] =
		"string" ^^^ StringType |
		"integer" ^^^ IntegerType |
		"uuid" ^^^ UUIDType |
		"date" ^^^ DateType
		
	lazy val columnModifier: PackratParser[ColumnTypeModifier] =
		pos ~ ("unique" | "required" | "optional" | "secret") ^^ {case p ~ m => ColumnTypeModifier( m, p.pos )}
		
	lazy val routesDefinition: PackratParser[List[RoutesDefinition]] =
		"route" ~> opt(uriPath) ~ (Indent ~> rep1(uriMapping) <~ Dedent) ^^ {
			case Some( base ) ~ mappings => List( RoutesDefinition( base, mappings ) )
			case None ~ mappings => List( RoutesDefinition( URIPath(Nil), mappings ) )
		}
		
	lazy val uriMapping: PackratParser[URIMapping] =
		httpMethod ~ expression <~ Newline ^^ {case method ~ action => URIMapping( method, URIPath(Nil), action )} |
		httpMethod ~ uriPath ~ expression <~ Newline ^^ {case method ~ uri ~ action => URIMapping( method, uri, action )}
		
	lazy val httpMethod: PackratParser[HTTPMethod] =
		("GET" | "POST" | "PUT" | "PATCH" | "DELETE") ^^ (HTTPMethod)
		
	lazy val uriPath: PackratParser[URIPath] =
		rep1sep(uriSegment, "/") ^^ (URIPath)
	
	lazy val uriSegment: PackratParser[URISegment] =
		ident ^^ (NameURISegment) |
		":" ~> ident ^^ (ParameterURISegment)
	
	lazy val expression: PackratParser[ExpressionAST] =
		numericLit ^^
			{n =>
				if (n matches ".*(\\.|e|E).*")
					LiteralExpression( n.toDouble )
				else
					LiteralExpression( n.toInt )
			} |
		expression ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {case name ~ args => ApplyExpression( name, args )} |
		ident ^^ (VariableExpression) |
		stringLit ^^ (LiteralExpression) |
		("true"|"false") ^^ (b => LiteralExpression( b.toBoolean )) |
		"null" ^^^ (LiteralExpression( null )) |
		"{" ~> repsep(pair, ",") <~ "}" ^^ (ObjectExpression)
	
	lazy val pair: PackratParser[(String, ExpressionAST)] = (ident|stringLit) ~ (":" ~> expression) ^^ {case k ~ v => (k, v)}
		
	lazy val functionLiteral =
		functionPart ^^ (p => FunctionExpression( List(p) )) |
		Indent ~> rep1(functionPart <~ Newline) <~ Dedent ^^ (FunctionExpression)
	
	lazy val functionPart =
		pattern ~ ("->" ~> expression) ^^ {case p ~ e => FunctionPart( p, e )}
		
	lazy val pattern: PackratParser[PatternAST] =
		ident ^^ (VariablePattern) |
		stringLit ^^ (StringPattern) |
		"(" ~> rep1sep( pattern, ",") <~ ")" ^^ (TuplePattern)
}

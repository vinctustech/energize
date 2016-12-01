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
		new IndentationLexical( false, true, List("[", "("), List("]", ")"), "//", "/*", "*/" )
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
						NumericLit( (intPart mkString "") :: frac :: exp :: Nil mkString "")} |
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
				"if", "then", "else", "elif", "true", "false", "or", "and", "not",
				"table", "unique", "required", "string", "optional", "integer", "secret", "route", "uuid", "date", "GET", "POST", "PUT", "PATCH", "DELETE"
				)
			delimiters += (
				"+", "*", "-", "/", "^", "(", ")", "[", "]", ",", "=", "==", "/=", "<", ">", "<=", ">=",
				":"
				)
		}

	def parse( r: Reader[Char] ) = phrase( source )( lexical.read(r) )

	import lexical.{Newline, Indent, Dedent}

	lazy val onl = opt(Newline)

	lazy val source =
		rep1(statement <~ Newline) |
		Newline ^^^ Nil

	lazy val statement: PackratParser[StatementAST] =
		tableDefinition |
		routesDefinition
	
	lazy val pos = positioned( success(new Positional{}) )
	
	lazy val tableDefinition: PackratParser[TableDefinition] =
		"table" ~> pos ~ ident ~ repsep(uriPath, ",") ~ (Indent ~> rep1(tableColumn) <~ Dedent) ^^ {
			case p ~ name ~ bases ~ columns => TableDefinition( p.pos, name, bases, columns )}
		
	lazy val tableColumn: PackratParser[TableColumn] =
		pos ~ ident ~ columnType ~ rep(columnModifier) <~ Newline ^^ {case p ~ name ~ typ ~ modifiers => TableColumn( p.pos, modifiers, typ, name )}

	lazy val columnType: PackratParser[ColumnType] =
		"string" ^^^ StringType |
		"integer" ^^^ IntegerType |
		"uuid" ^^^ UUIDType |
		"date" ^^^ DateType
		
	lazy val columnModifier: PackratParser[ColumnTypeModifier] =
		pos <~ "unique" ^^ {p => UniqueModifier( p.pos )} |
		pos <~ "required" ^^ {p => RequiredModifier( p.pos )} |
		pos <~ "optional" ^^ {p => OptionalModifier( p.pos )} |
		pos <~ "secret" ^^ {p => SecretModifier( p.pos )}
		
	lazy val routesDefinition: PackratParser[RoutesDefinition] =
		"route" ~> opt(uriPath) ~ (Indent ~> rep1(uriMapping) <~ Dedent) ^^ {
			case Some( base ) ~ mappings => RoutesDefinition( base, mappings )
			case None ~ mappings => RoutesDefinition( URIPath(Nil), mappings )
		}
		
	lazy val uriMapping: PackratParser[URIMapping] =
		httpMethod ~ expression <~ Newline ^^ {case method ~ action => URIMapping( method, URIPath(Nil), action )} |
		httpMethod ~ uriPath ~ expression <~ Newline ^^ {case method ~ uri ~ action => URIMapping( method, uri, action )}
		
	lazy val httpMethod: PackratParser[HTTPMethod] =
		"GET" ^^^ GETMethod |
		"POST" ^^^ POSTMethod |
		"PUT" ^^^ PUTMethod |
		"PATCH" ^^^ PATCHMethod |
		"DELETE" ^^^ DELETEMethod
		
	lazy val uriPath: PackratParser[URIPath] =
		rep1sep(uriSegment, "/") ^^ (URIPath)
	
	lazy val uriSegment: PackratParser[URISegment] =
		ident ^^ (NameURISegment) |
		":" ~> ident ^^ (ParameterURISegment)
	
	lazy val expression: PackratParser[ExpressionAST] =
		ident ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {case name ~ args => FunctionExpression( name, args )} |
		ident ^^ (VariableExpression) |
		stringLit ^^ (LiteralExpression) |
		("true"|"false") ^^ (b => LiteralExpression( b.toBoolean ))
}

case class PositionalString( s: String ) extends Positional
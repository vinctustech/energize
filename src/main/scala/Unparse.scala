package xyz.hyperreal.energize2

import xyz.hyperreal.bvm._


object Unparse extends App {

	def apply( ast: AST ): String = {
		var indentation = 0

		def indent( l: List[AST] ) = {
			indentation += 2

			val res = "\n" + block( l )

			indentation -= 2
			res
		}

		def block( l: List[AST] ) = l map (e => " "*indentation + unparse(e)) mkString "\n"

		def unparse( a: AST ): String =
			a match {
				case SourceAST( statements ) => block( statements )
				case ApplyExpressionAST( _, f, _, args, _ ) => unparse( f ) + '(' + args.map{case (_, e) => unparse( e )}.mkString(", ") + ')'
				case VariableExpressionAST( _, _, oname ) => oname
				case LiteralExpressionAST( s: String ) => '"' + s + '"'
				case LiteralExpressionAST( v ) => v.toString
				case ForExpressionAST( label, gen, body, els ) =>
					(if (label nonEmpty) label.get + ": " else "") +
					"for " +
					(gen map {
						case GeneratorExpressionAST( structure, _, traversable, filter) =>
							unparse( structure ) + " <- " + unparse( traversable ) + (if (filter isEmpty) "" else " if " + unparse( filter.get ))
					} mkString ", ") +
					" do " +
					unparse( body ) +
					(if (els isEmpty) "" else "else" + unparse( els get ))
				case VariableStructureAST( _, _, oname ) => oname
				case RangeExpressionAST( _, f, _, t, _, b, inc ) =>
					unparse( f ) + (if (inc) ".." else "..<") + unparse( t ) +
						(if (b == LiteralExpressionAST(1)) "" else " by " + unparse( b ))
				case BinaryExpressionAST( _, left, Symbol(name), _, _, right ) => unparse( left ) + name + unparse( right )
				case BlockExpressionAST( l ) => indent( l )
				case DefinedOptionExpressionAST( expr ) => "\\?" + unparse( expr )
				case DotExpressionAST( _, expr, _, Symbol(name) ) => unparse( expr ) + "." + name
				case _ => a.toString
			}

		unparse( ast ).replaceAll( """\n\s+\n""", "\n" )
	}

	val src =
		"""
			|for i <- 1..5
			|  write( i + 1 )
			|  write( i )
		""".stripMargin
	val p = new EnergizeParser

	println( Unparse(p.parseFromSource(io.Source.fromString(src), p.source)) )
}
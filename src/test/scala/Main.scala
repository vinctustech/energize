package xyz.hyperreal.informatio

import collection.immutable.PagedSeq
import util.parsing.input.PagedSeqReader

import xyz.hyperreal.indentation_lexical._


object Main extends App {
	
	val l = new IndentationLexical( false, true, List("[", "("), List("]", ")"), "//", "/*", "*/" )
	
	val s = l scan
		"""	|table users
				|  string name
				|  string dept
		""".stripMargin
		
	println( s )
	val p = new InformatioParser

	p.parse( new PagedSeqReader(PagedSeq.fromFile("t1.info")) ) match
	{
		case p.Success( tree, _ ) => println( tree )
		case p.NoSuccess( error, rest ) => println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
	}

}
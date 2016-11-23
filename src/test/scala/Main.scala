package xyz.hyperreal.informatio

import java.io.FileReader

import collection.immutable.PagedSeq
import util.parsing.input.{PagedSeqReader, CharSequenceReader}

import xyz.hyperreal.indentation_lexical._


object Main extends App {
	
	val p = new InformatioParser
	val ast =
		p.parse( new CharSequenceReader(io.Source.fromFile("t1.info").getLines.map(l => l + '\n').mkString) ) match {
			case p.Success( tree, _ ) => tree
			case p.NoSuccess( error, rest ) =>
				println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
				sys.exit
				sys.error( "" )
		}

	ast foreach {
		case TableDefinition( name, fields ) =>
			
		case RoutesDefinition( base, mappings ) =>
	}
}
package xyz.hyperreal.informatio

import java.sql._

import collection.immutable.PagedSeq
import util.parsing.input.{PagedSeqReader, CharSequenceReader}

import xyz.hyperreal.indentation_lexical._


object Main extends App {
	
	val p = new InformatioParser
	val ast =
		p.parse( new CharSequenceReader(io.Source.fromFile("t0.info").getLines.map(l => l + '\n').mkString) ) match {
			case p.Success( tree, _ ) => tree
			case p.NoSuccess( error, rest ) =>
				println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
				sys.exit
				sys.error( "" )
		}

		println( ast )
	Class.forName( "org.h2.Driver" )
	
	val conn = DriverManager.getConnection( "jdbc:h2:~/projects/informatio/test", "sa", "" )
	
	def problem( error: String ) {
		conn.close
		sys.error( error )
	}
	
	ast foreach {
		case TableDefinition( name, fields ) =>
			if (!conn.getMetaData.getTables( null, null, name, null ).next) {
				val f =
					fields map {
						case TableField( modifiers, typ, name ) =>
							val t =
								typ match {
									case StringType => "VARCHAR(255)"
									case UUIDType => "UUID"
									case DateType => "DATE"
								}
								
							name + " " + t
					} mkString ", "
				
				val com = "CREATE TABLE " + name + "(" + "id INT AUTO_INCREMENT PRIMARY KEY, " + f + ")"
				
				println( com )
			}
		case RoutesDefinition( base, mappings ) =>
			
	}
	
	conn.close
}
package xyz.hyperreal.informatio

import java.sql._
import java.io.File

import util.parsing.input.CharSequenceReader
import collection.mutable.ListBuffer


object Interpreter {
	
	def apply( src: String ): List[Route] = apply( io.Source.fromString(src) )
	
	def apply( src: File ): List[Route] = apply( io.Source.fromFile(src) )
	
	def apply( src: io.Source ): List[Route] = {
		val p = new InformatioParser
		val ast =
			p.parse( new CharSequenceReader(src.getLines.map(l => l + '\n').mkString) ) match {
				case p.Success( tree, _ ) => tree
				case p.NoSuccess( error, rest ) =>
					println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
					sys.exit
					sys.error( "" )
			}
			
		Class.forName( "org.h2.Driver" )
		
		val conn = DriverManager.getConnection( "jdbc:h2:~/projects/informatio/test", "sa", "" )
		val statement = conn.createStatement
		
		def problem( error: String ) {
			conn.close
			sys.error( error )
		}
		
		val routes = new ListBuffer[Route]
		
		ast foreach {
			case TableDefinition( name, bases, fields ) =>
				if (!conn.getMetaData.getTables( null, null, name.toUpperCase, null ).next) {
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
					statement.execute( com )
				}

				for (URIPath( base ) <- bases)
					routes ++=
						Interpreter(
							"""
							|routes <base>/<table>
							|  GET    :id                          query( "select from <table> where id = '$id'" )
							|  GET                                 query( "select from <table>" )
							|  POST                                insert( <table>, json )
							|  DELETE :id                          command( "delete from <table> where id = '$id'" )
							""".stripMargin.replaceAll("<table>", name).replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString "/")
						)
					
			case RoutesDefinition( URIPath(base), mappings ) =>
				
				mappings foreach {
					case URIMapping( GETMethod, URIPath(path), action ) => routes += Route( "GET", base ++ path, action )
					case URIMapping( POSTMethod, URIPath(path), action ) => routes += Route( "POST", base ++ path, action )
					case URIMapping( PUTMethod, URIPath(path), action ) => routes += Route( "PUT", base ++ path, action )
					case URIMapping( DELETEMethod, URIPath(path), action ) => routes += Route( "DELETE", base ++ path, action )
					case _ => sys.error( "unknown method" )
				}
		}
		
		conn.close
		routes.toList
	}

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )
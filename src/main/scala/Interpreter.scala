package xyz.hyperreal.informatio

import java.io.File

import util.parsing.input.CharSequenceReader
import collection.mutable.{LinkedHashMap, HashMap, ListBuffer}

import xyz.hyperreal.table.TextTable


object Interpreter {
	
	def apply( src: String ): (Map[String, Table], List[Route]) = apply( io.Source.fromString(src) )
	
	def apply( src: File ): (Map[String, Table], List[Route]) = apply( io.Source.fromFile(src) )
	
	def apply( src: io.Source ): (Map[String, Table], List[Route]) = {
		def problem( error: String ) = {
			sys.error( error )
		}
		
		val p = new InformatioParser
		val ast =
			p.parse( new CharSequenceReader(src.getLines.map(l => l + '\n').mkString) ) match {
				case p.Success( tree, _ ) => tree
				case p.NoSuccess( error, rest ) =>
					println( rest.pos.line + ": " + error + "\n" + rest.pos.longString )
					sys.exit
					problem( "" )
			}
		
		val tables = new HashMap[String, LinkedHashMap[String, Column]]
		val routes = new ListBuffer[Route]
		
		ast foreach {
			case TableDefinition( name, bases, columns ) =>
				if (tables contains name)
					problem( "table defined twice: " + name )

				val cols = new LinkedHashMap[String, Column]
				val f =
					columns map {
						case TableColumn( modifiers, typ, cname ) =>
							if (cols contains cname)
								problem( "column defined twice: " + cname )
								
							val t =
								typ match {
									case StringType => "VARCHAR(255)"
									case IntegerType => "INT"
									case UUIDType => "UUID"
									case DateType => "DATE"
								}
							var secret = false
							var required = false
							var optional = false
							var unique = false
							var m = ""
							
							modifiers foreach {
								case UniqueModifier =>
									if (unique)
										problem( "modifier 'unique' encountered more than once: " + name + "/" + cname )
										
									unique = true
									m += " unique"
								case RequiredModifier =>
									if (required)
										problem( "modifier 'required' encountered more than once: " + name + "/" + cname )
										
									if (optional)
										problem( "modifier 'required' encountered along with 'optional': " + name + "/" + cname )
										
									m += " not null"
									required = true
								case OptionalModifier =>
									if (optional)
										problem( "modifier 'optional' encountered more than once: " + name + "/" + cname )
										
									if (required)
										problem( "modifier 'optional' encountered along with 'required': " + name + "/" + cname )
										
									optional = true
								case SecretModifier =>
									if (secret)
										problem( "modifier 'secret' encountered more than once: " + name + "/" + cname )
										
									secret = true	
							}
							
							cols(cname) = Column( cname, typ, secret, required )
							cname + " " + t + m
					} mkString ", "
				
				tables(name) = cols
					
// 				val rs = connection.getMetaData.getTables( null, "PUBLIC", name.toUpperCase, null )
// 				
// 				while (rs.next)
// 					println( for (i <- 1 to 11) yield rs.getObject(i) )
				if (!connection.getMetaData.getTables( null, "PUBLIC", name.toUpperCase, null ).next) {
					println( "creating table '" + name.toUpperCase + "'" )
					statement.execute( "CREATE TABLE " + name + "(id INT AUTO_INCREMENT PRIMARY KEY, " + f + ")" )
				}
				
				for (URIPath( base ) <- bases) {
					val (_, r) = Interpreter(
						"""
						|route <base>/<table>
						|  GET    :id    query( "select * from <table> where id = '$id';" )
						|  GET           query( "select * from <table>;" )
						|  POST          insert( <table>, json )
						|  PATCH  :id    update( <table>, json, id )
						|  DELETE :id    command( "delete from <table> where id = '$id';" )
						""".stripMargin.replaceAll("<table>", name).replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString "/")
					)
					
					routes ++= r
				}
					
			case RoutesDefinition( URIPath(base), mappings ) =>
				mappings foreach {
					case URIMapping( GETMethod, URIPath(path), action ) => routes += Route( "GET", base ++ path, action )
					case URIMapping( POSTMethod, URIPath(path), action ) => routes += Route( "POST", base ++ path, action )
					case URIMapping( PUTMethod, URIPath(path), action ) => routes += Route( "PUT", base ++ path, action )
					case URIMapping( PATCHMethod, URIPath(path), action ) => routes += Route( "PATCH", base ++ path, action )
					case URIMapping( DELETEMethod, URIPath(path), action ) => routes += Route( "DELETE", base ++ path, action )
					case _ => problem( "unknown method" )
				}
		}
		
		val tableMap = tables.map {
			case (tname, tinfo) => {
				val cnames = new ListBuffer[String]
				
				tinfo foreach {
					case (cname, cinfo) =>
						cnames += cname
				}
				
				(tname, Table( tname, cnames.toList, tinfo.toMap ))
			}
		} toMap
		
		(tableMap, routes.toList)
	}

}

case class Route( method: String, path: List[URISegment], action: ExpressionAST )
	
case class Table( name: String, names: List[String], columns: Map[String, Column] )
	
case class Column( name: String, typ: ColumnType, secret: Boolean, required: Boolean )
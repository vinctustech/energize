package xyz.hyperreal

import java.sql._

import util.parsing.input.{Position}
import collection.mutable.{LinkedHashMap, HashMap, ListBuffer}

import xyz.hyperreal.table.TextTable


package object cras {
	
//	type JSON = Map[String, Any]
	
	def dbconnect( dbfile: String, memory: Boolean = false ) = {
		Class.forName( "org.h2.Driver" )
		
		val connection = DriverManager.getConnection( s"jdbc:h2${if (memory) ":mem:" else ":"}" + dbfile, "sa", "" )
		
		(connection, connection.createStatement)
	}
	
	def problem( pos: Position, error: String ) = sys.error( pos.line + ": " + error + "\n" + pos.longString )
	
	def configure( src: io.Source, connection: Connection, statement: Statement ): Env = {
		val p = new CrasParser
		val ast = p.parseFromSource( src, p.source )
		val tables = new HashMap[String, Table]
		val routes = new ListBuffer[Route]
		val defines = new HashMap[String, Any]
		
		def env = Env( tables.toMap, routes.toList, Builtins.map ++ defines, connection, statement )
		
		def traverseDefinitions( list: List[AST] ) = list foreach interpretDefinitions
		
		def interpretDefinitions( ast: AST ): Unit =
			ast match {
				case SourceAST( list ) => traverseDefinitions( list )
				case d@VariableDefinition( name, expr ) =>
					if (defines contains name)
						problem( d.pos, s"'$name' already defined" )
						
					defines(name) = new Variable( env.deref(expr) )
				case d@ValueDefinition( name, expr ) =>
					if (defines contains name)
						problem( d.pos, s"'$name' already defined" )
						
					defines(name) = env.deref( expr )
				case d@FunctionDefinition( name, function ) =>
					if (defines contains name)
						problem( d.pos, s"'$name' already defined" )
						
					defines(name) = function
				case TableDefinition( pos, name, bases, columns ) =>
					if (tables contains name.toUpperCase)
						problem( pos, s"'$name' already defined" )
					
					val cols = new LinkedHashMap[String, Column]
					
					columns foreach {
						case col@TableColumn( modifiers, typ, cname ) =>
							if (cols contains cname.toUpperCase)
								problem( col.pos, s"column '$cname' defined twice" )
								
							var secret = false
							var required = false
							var optional = false
							var unique = false
							var indexed = false
							
							modifiers foreach {
								case tm@ColumnTypeModifier( "indexed" ) =>
									if (indexed)
										problem( tm.pos, "modifier 'indexed' encountered more than once" )
										
									indexed = true
								case tm@ColumnTypeModifier( "unique" ) =>
									if (unique)
										problem( tm.pos, "modifier 'unique' encountered more than once" )
										
									unique = true
								case tm@ColumnTypeModifier( "required" ) =>
									if (required)
										problem( tm.pos, "modifier 'required' encountered more than once" )
										
									if (optional)
										problem( tm.pos, "modifier 'required' encountered along with 'optional'" )
										
									required = true
								case tm@ColumnTypeModifier( "optional" ) =>
									if (optional)
										problem( tm.pos, "modifier 'optional' encountered more than once" )
										
									if (required)
										problem( tm.pos, "modifier 'optional' encountered along with 'required'" )
										
									optional = true
								case tm@ColumnTypeModifier( "secret" ) =>
									if (secret)
										problem( tm.pos, "modifier 'secret' encountered more than once" )
										
									secret = true
								case tm@ColumnTypeModifier( m ) =>
									problem( tm.pos, s"unknown modifier '$m'" )
							}
							
							cols(cname.toUpperCase) = Column( cname, typ, secret, required, unique, indexed )
					}
		
					tables(name.toUpperCase) = Table( name, cols map {case (_, cinfo) => cinfo.name} toList, cols.toMap )
					
					if (bases isEmpty) {
						val Env( _, r, _, _, _ ) =
							configure( io.Source.fromString(Builtins.routes.replaceAll("<base>", "").replaceAll("<resource>", name)), null, null )
							
						routes ++= r
					} else {
						for (URIPath( base ) <- bases) {
							val Env( _, r, _, _, _ ) = configure( io.Source.fromString(Builtins.routes.replaceAll("<resource>", name).
								replaceAll("<base>", base map {case NameURISegment(segment) => segment} mkString ("/", "/", ""))), null, null )
							
							routes ++= r
						}
					}
				case RoutesDefinition( URIPath(base), mappings ) =>
					mappings foreach {
						case URIMapping( HTTPMethod(method), URIPath(path), action ) =>
							routes += Route( method, base ++ path, action )
					}
				case _ =>
			}
		
		def traverseExpressions( list: List[AST] ) = list foreach interpretExpressions
		
		def interpretExpressions( ast: AST ): Unit =
			ast match {
				case SourceAST( list ) => traverseExpressions( list )
				case ExpressionStatement( expr ) => env.deref( expr )
				case _ =>
			}
			
		interpretDefinitions( ast )	
		
		val sorted =
			TableSorter.sort( tables.values ) match {
				case None => sys.error( "resources cannot be topologically ordered" )
				case Some( s ) => s
			}
			
		if (!tables.isEmpty && !connection.getMetaData.getTables( null, "PUBLIC", tables.head._1, null ).next) {
			statement.execute( H2Database.create(sorted) )
		}
		
		interpretExpressions( ast )
		env
	}
	
	def escapeQuotes( s: String ): String = s replace ("'", "''")
		
	def escapeQuotes( json: Map[String, Any] ): Map[String, Any] =
		json map {case (k, v) =>
			(k, if (v.isInstanceOf[String]) escapeQuotes( v.asInstanceOf[String] ) else v)
		}
}
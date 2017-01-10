package xyz.hyperreal.cras

import java.sql._

import collection.mutable.{HashMap, LinkedHashMap, ListBuffer}


object Cras {

	def dbconnect: (Connection, Statement) = {
		val driver = DATABASE.getString( "driver" )
		val url = DATABASE.getString( "url" )
		val username = DATABASE.getString( "username" )
		val password = DATABASE.getString( "password" )

		dbconnect( driver, url, username, password )
	}

	def h2connect( file: String ) = dbconnect( "org.h2.Driver", s"jdbc:h2:$file", "sa", "" )

	def dbconnect( driver: String, url: String, username: String, password: String ) = {

    Class.forName( driver )
		
		val connection = DriverManager.getConnection( url, username, password )
		
		(connection, connection.createStatement)
	}

	def configure( src: io.Source, connection: Connection, statement: Statement ): Env = {
		val p = new CrasParser
		val ast = p.parseFromSource( src, p.source )
		val tables = new HashMap[String, Table]
		val routes = new ListBuffer[Route]
		val defines = new HashMap[String, Any]
		val db = if (connection eq null) null else Database( connection.getMetaData.getDriverName )

		def env = Env( tables.toMap, routes.toList, Builtins.map ++ defines, connection, statement, db )
		
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
					if (tables contains db.desensitize( name ))
						problem( pos, s"'$name' already defined" )
					
					val cols = new LinkedHashMap[String, Column]
					
					columns foreach {
						case col@TableColumn( modifiers, typ, cname ) =>
							if (cols contains db.desensitize( cname ))
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
							
							cols(db.desensitize( cname )) = Column( cname, typ, secret, required, unique, indexed )
					}
		
					tables(db.desensitize( name )) = Table( name, cols map {case (_, cinfo) => cinfo.name} toList, cols.toMap, null )
					
					if (bases isEmpty) {
						val Env( _, r, _, _, _, _ ) =
							configure( io.Source.fromString(Builtins.routes.replaceAll("<base>", "").replaceAll("<resource>", name)), null, null )
							
						routes ++= r
					} else {
						for (URIPath( base ) <- bases) {
							val Env( _, r, _, _, _, _ ) = configure( io.Source.fromString(Builtins.routes.replaceAll("<resource>", name).
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

		if (tables.nonEmpty && !connection.getMetaData.getTables( null, db.publicSchema, tables.head._1, null ).next) {
//			print( xyz.hyperreal.table.TextTable(connection.getMetaData.getTables( null, null, tables.head._1, null )) )
			statement.execute( db.create(sorted) )
		}
		
		tables.values foreach {
			case t@Table( name, cnames, _, _ ) =>
				val columns = cnames mkString ","
				val values = Seq.fill( cnames.length )( "?" ) mkString ","
				
				t.preparedInsert = connection.prepareStatement( s"INSERT INTO $name ($columns) VALUES ($values)" )
		}
		
		interpretExpressions( ast )
		env
	}
}
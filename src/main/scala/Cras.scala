package xyz.hyperreal.cras

import java.sql._

import collection.mutable.{HashMap, LinkedHashMap, ListBuffer}

import xyz.hyperreal.json.{DefaultJSONReader, JSON}

import org.h2.jdbcx.JdbcConnectionPool


object Cras {

	def dbconnect: (Connection, Statement) = {
		val name = DATABASE.getString( "name" )
		val driver = DATABASE.getString( "driver" )
		val url = DATABASE.getString( "url" )
		val user = DATABASE.getString( "user" )
		val password = DATABASE.getString( "password" )

		dbconnect( name, driver, url, user, password )
	}

	def dbconnect( name: String, driver: String, url: String, user: String, password: String ) = {
    Class.forName( driver )
		
		val connection =
//				JdbcConnectionPool.create( url, user, password ).getConnection
			if (user eq null)
				DriverManager.getConnection( url )
			else
				DriverManager.getConnection( url, user, password )
		
		(connection, connection.createStatement)
	}

	def configure( src: io.Source, connection: Connection, statement: Statement ): Env = {
		val p = new CrasParser

		configure( p.parseFromSource(src, p.source), connection: Connection, statement: Statement )
	}

	def configure( src: String, connection: Connection, statement: Statement ): Env = {
		val json = DefaultJSONReader.fromString( src )
		val ast =
			SourceAST(
				json.getList( "declarations" ) map {
					case o: JSON =>
						o getString "type" match {
							case "resource" =>
								TableDefinition( Protection(None), null, o getString "name", null, null, resource = true )
						}
				} )

		configure( ast, connection: Connection, statement: Statement )
	}

	def configure( ast: SourceAST, connection: Connection, statement: Statement ): Env = {
		val tables = new HashMap[String, Table]
		val routes = new ListBuffer[Route]
		val defines = new HashMap[String, Any]
		val db = if (connection eq null) null else Database( DATABASE.getString("name") )

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
				case TableDefinition( _, pos, name, bases, columns, resource ) =>
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
		
					tables(db.desensitize( name )) = Table( name, cols map {case (_, cinfo) => cinfo.name} toList, cols.toMap, resource, null )

					if (resource)
						if (bases isEmpty)
							routes ++= configure( io.Source.fromString( Builtins.routes.replaceAll( "<base>", "" ).replaceAll( "<resource>", name ) ), null, null ).routes
						else
							for (URIPath( base ) <- bases)
								routes ++= configure( io.Source.fromString( Builtins.routes.replaceAll( "<resource>", name ).
									replaceAll( "<base>", base map { case NameURISegment( segment ) => segment } mkString("/", "/", "") ) ), null, null ).routes

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

		tables.values foreach {
			case Table( _, _, columns, _, _ ) =>
				columns.values foreach {
					case Column( _, t@SingleReferenceType(table, _), _, _, _, _ ) =>
						t.ref = tables.getOrElse( db.desensitize(table), problem(t.pos, s"'$table' not found") )
					case Column( _, t@ManyReferenceType(table, _), _, _, _, _ ) =>
						t.ref = tables.getOrElse( db.desensitize(table), problem(t.pos, s"'$table' not found") )
					case _ =>
				}
		}

		val sorted = TableSorter.sort( tables.values ) getOrElse sys.error( "resources cannot be topologically ordered" )

		if (tables.nonEmpty && !connection.getMetaData.getTables( null, db.publicSchema, tables.head._1, null ).next) {
//			print( xyz.hyperreal.table.TextTable(connection.getMetaData.getTables( null, null, tables.head._1, null )) )
//			println( db.create(sorted) )
			statement.execute( db.create(sorted) )
		}
		
		tables.values foreach {
			case t@Table( name, cnames, cols, _, _ ) =>
				val cnames1 = cnames filterNot (c => cols(db.desensitize(c)).typ.isInstanceOf[ManyReferenceType])
				val columns = cnames1 mkString ","
				val values = Seq.fill( cnames1.length )( "?" ) mkString ","
				
				t.preparedInsert = connection.prepareStatement( s"INSERT INTO $name ($columns) VALUES ($values)" )
		}
		
		interpretExpressions( ast )

//		if (src ne Builtins.control)
//			routes ++= configure( Builtins.control, null, null ).routes

		env
	}
}
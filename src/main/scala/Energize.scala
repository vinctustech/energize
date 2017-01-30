package xyz.hyperreal.energize

import java.sql._
import java.time.{Instant, ZoneOffset}

import collection.mutable.{HashMap, LinkedHashMap, ListBuffer}
import collection.JavaConverters._
import xyz.hyperreal.json.{DefaultJSONReader, JSON}
import org.h2.jdbcx.JdbcConnectionPool


object Energize {

	def dbconnect: (Connection, Statement, Database) = {
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
		
		(connection, connection.createStatement, Database( name ))
	}

	def configure( src: io.Source, connection: Connection, statement: Statement, database: Database ): Env = {
		val p = new EnergizeParser

		configure( p.parseFromSource(src, p.source), connection, statement, database )
	}

//	def configure( src: String, connection: Connection, statement: Statement, database: Database ): Env = {
//		val json = DefaultJSONReader.fromString( src )
//		val ast =
//			SourceAST(
//				json.getList( "declarations" ) map {
//					case o: JSON =>
//						o getString "type" match {
//							case "resource" =>
//								TableDefinition( Protection(None), null, o getString "name", null, null, resource = true )
//						}
//				} )
//
//		configure( ast, connection, statement, database )
//	}

	def configure( ast: SourceAST, connection: Connection, statement: Statement, db: Database ): Env =
		configure( ast, connection, statement, db, false )

	private def configure_( src: String, connection: Connection, statement: Statement, database: Database ): Env = {
		val p = new EnergizeParser

		configure( p.parseFromSource( io.Source.fromString( src ), p.source ), connection, statement, database, true )
	}

	private def configure( ast: SourceAST, connection: Connection, statement: Statement, db: Database, internal: Boolean ): Env = {
		val tables = new HashMap[String, Table]
		val routes = new ListBuffer[Route]
		val defines = new HashMap[String, Any]

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
					var mtm = false

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

							if (typ.isInstanceOf[ManyReferenceType])
								mtm = true

							cols(db.desensitize( cname )) = Column( cname, typ, secret, required, unique, indexed )
					}

					tables(db.desensitize( name )) = Table( name, cols map {case (_, cinfo) => cinfo} toList, cols.toMap, resource, mtm, null )

					if (resource) {
						val mtm = cols.values exists (c => c.typ.isInstanceOf[ManyReferenceType])

						if (bases isEmpty) {
							routes ++= configure_( Builtins.routes.replaceAll( "<base>", "" ).replaceAll( "<resource>", name ), connection, statement, db ).routes

							if (mtm)
								routes ++= configure_( Builtins.mtmroutes.replaceAll( "<base>", "" ).replaceAll( "<resource>", name ), connection, statement, db ).routes
						} else
							for (URIPath( base ) <- bases) {
								routes ++= configure_( Builtins.routes.replaceAll( "<resource>", name ).
									replaceAll( "<base>", base map { case NameURISegment( segment ) => segment } mkString("/", "/", "") ), connection, statement, db ).routes

								if (mtm)
									routes ++= configure_( Builtins.mtmroutes.replaceAll( "<resource>", name ).
										replaceAll( "<base>", base map { case NameURISegment( segment ) => segment } mkString("/", "/", "") ), connection, statement, db ).routes
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

		if (!internal) {
			val e = configure_( Builtins.special.
				replaceAll( "<base>", AUTHORIZATION.getString("base") ).
				replaceAll( "<email>", ADMIN.getString("email") ).
				replaceAll( "<password>", ADMIN.getString("password") ), connection, statement, db )

			routes ++= e.routes

			for ((k, v) <- e.tables)
				tables get k match {
					case Some( Table(name, columns, columnMap, _, mtm, _ )) =>
						tables(k) = Table(name, v.columns ++ columns, v.columnMap ++ columnMap, v.resource, v.mtm || mtm, null )
					case None => tables(k) = v
				}

			tables.values foreach {
				case Table( _, columns, _, _, _, _ ) =>
					columns foreach {
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
				case t@Table( name, cols, _, _, _, _ ) =>
					val cnames1 = cols filterNot (c => c.typ.isInstanceOf[ManyReferenceType]) map (c => c.name)
					val columns = cnames1 mkString ","
					val values = Seq.fill( cnames1.length )( "?" ) mkString ","

					t.preparedInsert = connection.prepareStatement( s"INSERT INTO $name ($columns) VALUES ($values)" )
			}

			val en = env
			val admin =
				Map( ADMIN.entrySet.asScala.toList.map( e =>
					(e.getKey, ADMIN.getValue(e.getKey).unwrapped) match {
						case (k, o: java.util.List[_]) => (k, o.asScala)
						case (k, o) => (k, o)
					}
				) :+ "createdTime" -> SupportFunctions.now(en): _* )

			CommandFunctions.insert( en, tables(db.desensitize("users")), admin )
		}

		interpretExpressions( ast )

		env
	}
}

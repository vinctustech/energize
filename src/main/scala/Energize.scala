package xyz.hyperreal.energize

import java.sql._

import collection.mutable.{HashMap, LinkedHashMap, ArrayBuffer, ListBuffer}
import collection.JavaConverters._

import xyz.hyperreal.json.{JSON, DefaultJSONReader}

import org.mindrot.jbcrypt.BCrypt


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

	def configure( src: io.Source, connection: Connection, statement: Statement, database: Database ): Environment = {
		val p = new EnergizeParser

		configure( p.parseFromSource(src, p.source), connection, statement, database )
	}

	def configureFromJSON( src: io.Source, connection: Connection, statement: Statement, database: Database ): Environment = {
		val s = src mkString
		val json = DefaultJSONReader.fromString( s )
		val decl = new ListBuffer[StatementAST]

		for ((k, v) <- json)
			k match {
				case "tables" =>
					for (tab <- v.asInstanceOf[List[JSON]]) {
						val cols = new ListBuffer[TableColumn]

						for (c <- tab.getList[JSON]( "fields" )) {
							val typ = c.getMap( "type" )
							val cat = typ getString "category"
							val styp = typ getString "type"
							val ctyp =
								cat match {
									case "primitive" =>
										styp match {
											case "string" => StringType
											case "integer" => IntegerType
											case "long" => LongType
											case "uuid" => UUIDType
											case "date" => DateType
											case "datetime" => DatetimeType
											case "time" => TimeType
											case "timestamp" => TimestampType
											case "binary" => BinaryType
											case "blob" => BLOBType( 'base64 )
											case "float" => FloatType
											case "decimal" =>
												val List( prec, scale ) = typ.getList[Int]( "parameters" )

												DecimalType( prec, scale )
											case "media" => MediaType( None, None, Int.MaxValue )
										}
								}

							cols += TableColumn( c getString "name", ctyp, Nil )
						}

						decl += TableDefinition( None, null, tab getString "name", Nil, cols toList, tab.getBoolean("resource") )
					}
			}

		configure( SourceAST(decl toList), connection, statement, database )
	}

	def configure( ast: SourceAST, connection: Connection, statement: Statement, db: Database ): Environment =
		configure( ast, connection, statement, db, false )

	private def configure_( src: String, connection: Connection, statement: Statement, database: Database ): Environment = {
		val p = new EnergizeParser

		configure( p.parseFromSource( io.Source.fromString( src ), p.source ), connection, statement, database, true )
	}

	private def configure( ast: SourceAST, connection: Connection, statement: Statement, db: Database, internal: Boolean ): Environment = {
		val tables = new HashMap[String, Table]
		val routes = new ArrayBuffer[Route]
		val defines = new HashMap[String, Any]

		def env = new Environment( tables.toMap, routes.toList, Builtins.map ++ defines, Builtins.sys, connection, statement, db, Map.empty, Map.empty )
		
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
				case TableDefinition( protection, pos, name, bases, columns, resource ) =>
					var mtm = false

					if (tables contains db.desensitize( name ))
						problem( pos, s"'$name' already defined" )
					
					val cols = new LinkedHashMap[String, Column]
					
					columns foreach {
						case col@TableColumn( cname, typ, modifiers ) =>
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
						val authorize =
							protection match {
								case None => ""
								case Some( None ) => "protected"
								case Some( Some(g) ) => s"protected ($g)"
							}

						if (bases isEmpty) {
							routes ++= configure_( Builtins.routes.replaceAll("<base>", "").replaceAll("<resource>", name).replaceAll("<authorize>", authorize),
								connection, statement, db ).routes

							if (mtm)
								routes ++= configure_( Builtins.mtmroutes.replaceAll("<base>", "").replaceAll("<resource>", name).replaceAll("<authorize>", authorize),
									connection, statement, db ).routes
						} else
							for (URIPath( base ) <- bases) {
								routes ++= configure_( Builtins.routes.replaceAll( "<resource>", name ).
									replaceAll( "<base>", base map {case NameURISegment( segment ) => segment} mkString("/", "/", "") ).replaceAll("<authorize>", authorize),
									connection, statement, db ).routes

								if (mtm)
									routes ++= configure_( Builtins.mtmroutes.replaceAll( "<resource>", name ).
										replaceAll( "<base>", base map {case NameURISegment( segment ) => segment} mkString("/", "/", "") ).replaceAll("<authorize>", authorize),
										connection, statement, db ).routes
							}
					}
				case RoutesDefinition( URIPath(base), mappings, protection ) =>
					val block = new ArrayBuffer[Route]

					mappings foreach {
						case URIMapping( HTTPMethod(method), URIPath(path), action ) =>
							block += Route( method, base ++ path, action )

							if (protection nonEmpty)
								for ((Route( method, path, action), i) <- block zipWithIndex)
									block(i) = Route( method, path,
										CompoundExpression(ApplyExpression(VariableExpression("authorize"), null, List(LiteralExpression(protection.get))), action) )
					}

					block ++=: routes
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
//				println( db.create(sorted) )
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
						case ("password", p: String) => ("password", BCrypt.hashpw( p, BCrypt.gensalt ))
						case (k, o) => (k, o)
					}
				) :+ "createdTime" -> UtilityFunctions.now(en): _* )

			CommandFunctions.insert( en, tables(db.desensitize("users")), admin )
		}

		interpretExpressions( ast )

		env
	}
}

package xyz.hyperreal.energize

import java.sql._

import collection.mutable.{HashSet, HashMap, LinkedHashMap, ArrayBuffer, ListBuffer}
import collection.JavaConverters._
import util.parsing.input.Reader

import xyz.hyperreal.json.{JSON, DefaultJSONReader}
import xyz.hyperreal.importer.Importer

import org.mindrot.jbcrypt.BCrypt


object Energize {

	val MIME = """([a-z+]+)/(\*|[a-z+]+)"""r

	def dbconnect: (Connection, Statement, Database) = {
		val url = DATABASE.getString( "url" )
		val user = DATABASE.getString( "user" )
		val password = DATABASE.getString( "password" )

		dbconnect( url, user, password )
	}

	def dbconnect( url: String, user: String, password: String ): (Connection, Statement, Database) = {
		val name = DATABASE.getString( "name" )
		val driver = DATABASE.getString( "driver" )

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

	def configure( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ): Environment = {
		val p = new EnergizeParser

		configure( p.parseFromSource(src, p.source), connection, statement, database, key )
	}

	def primitive( typ: JSON ): PrimitiveColumnType =
		typ getString "type" match {
			case "string" => StringType
			case "text" => TextType
			case "boolean" => BooleanType
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
//			case "enum" => EnumType( _, typ.getList[String]("parameters").toVector )
			case "media" =>
				val List( allowed, limit ) = typ.getList[AnyRef]( "parameters" )
				val allowed1 =
					for (a <- allowed.asInstanceOf[List[String]])
						yield
							a match {
								case MIME( typ, subtype ) => MimeType( typ, subtype )
								case _ => sys.error( s"bad MIME type: $a" )
							}

				if (limit eq null)
					MediaType( allowed1, null, Int.MaxValue )
				else
					MediaType( allowed1, null, limit.asInstanceOf[Int] )
		}

	def parsePath( path: String ): Option[URIPath] =
		if (path endsWith "/")
			None
		else
			path split "/" toList match {
				case Nil|List( "" ) => None
				case a =>
					if (a.head != "")
						None
					else {
						val tail = a.tail

						if (tail contains "")
							None
						else
							Some( URIPath(tail map NameURISegment) )
					}
			}

	def configureFromJSON( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ): Environment = {
		val s = src mkString
		val json = DefaultJSONReader.fromString( s )
		val decl = new ListBuffer[StatementAST]

		for ((k, v) <- json)
			k match {
				case "tables" =>
					for (tab <- v.asInstanceOf[List[JSON]]) {
						val pro =
							tab get "protection" match {
								case None => None
								case Some( groups: List[_] ) => Some( groups.asInstanceOf[List[String]].headOption )
								case Some( _ ) => sys.error( "protection expected to be a list" )
							}
						val priv =
							tab get "private" match {
								case None => false
								case Some( _ ) => true
							}
						val base =
							if (tab contains "base")
								parsePath( tab getString "base" )
							else
								None
						val cols = new ListBuffer[TableColumn]

						for (c <- tab.getList[JSON]( "fields" )) {
							val typ = c getMap "type"
							val cat = typ getString "category"
							val ctyp =
								cat match {
									case "primitive" => primitive( typ )
									case "array" => ArrayType( primitive(typ), null, null, 1 )
									case "one-to-many" => SingleReferenceType( typ getString "type", null )
									case "many-to-many" => ManyReferenceType( typ getString "type", null )
								}
							val modifiers =
								if (c contains "modifiers")
									c getList[String] "modifiers" map ColumnTypeModifier
								else
									Nil
							val validators =
								if (c contains "validators")
									c getList[String] "validators"
								else
									Nil

							cols += TableColumn( c getString "name", ctyp, modifiers, validators map EnergizeParser.parseValidator )
						}

						decl += TableDefinition( pro, null, tab getString "name", base, cols toList, tab.getBoolean("resource") )
					}
				case "routes" =>
					for (routes <- v.asInstanceOf[List[JSON]]) {
						val base =
							if (routes contains "base")
								parsePath( routes getString "base" ).get
							else
								URIPath( Nil )
						val protection =
							if (routes contains "protection") {
								val p = routes getString "protection"

								if (p eq null)
									Some( None )
								else
									Some( Some(p) )
							} else
								None
						val mappings =
							for (m <- routes.getList[JSON]( "mappings" ))
								yield {
									val method = HTTPMethod( m getString "method" )
									val path = parsePath( m getString "path" ).get
									val action =
										m getString "language" match {
											case "ESL" => parseExpression( m getString "action" )
											case "ECMAScript" => JavaScriptExpression( m getString "action" )
										}
									URIMapping( method, path, action )
								}

						decl += RoutesDefinition( base, protection, mappings )
					}
			}

		configure( SourceAST(decl toList), connection, statement, database, key )
	}

//	def configureFromDB( src: Reader[Char], connection: Connection, statement: Statement, database: Database, key: String ): Environment = {
//		val db = new Importer().importFromReader( src )
//		val decl = new ListBuffer[StatementAST]
//
//		for ((name, table) <- db) {
//			val cols = new ListBuffer[TableColumn]
//
//			for (c <- table.header) {
//				val typ = c getMap "type"
//				val cat = typ getString "category"
//				val ctyp =
//					cat match {
//						case "primitive" => primitive( typ )
//						case "array" => ArrayType( primitive(typ), null, null, 1 )
//						case "one-to-many" => SingleReferenceType( typ getString "type", null )
//						case "many-to-many" => ManyReferenceType( typ getString "type", null )
//					}
//				val modifiers =
//					if (c contains "modifiers")
//						c getList[String] "modifiers" map ColumnTypeModifier
//					else
//						Nil
//
//				cols += TableColumn( c getString "name", ctyp, modifiers )
//			}
//
//			decl += TableDefinition( None, null, name, null, cols toList, true )
//		}
//
//		configure( SourceAST(decl toList), connection, statement, database, key )
//	}

	def configure( ast: SourceAST, connection: Connection, statement: Statement, db: Database, key: String ): Environment =
		configure( ast, connection, statement, db, key, false )

	private def configure_( src: String, connection: Connection, statement: Statement, database: Database ): Environment = {
		val p = new EnergizeParser

		configure( p.parseFromSource(io.Source.fromString( src ), p.source), connection, statement, database, null, true )
	}

	private def configure( ast: SourceAST, connection: Connection, statement: Statement, db: Database, key: String, internal: Boolean ): Environment = {
		val tables = new HashMap[String, Table]
		val routes = new ArrayBuffer[Route]
		val defines = new HashMap[String, Any]

		def env = new Environment( tables.toMap, routes.toList, Builtins.map ++ defines, Builtins.sys, connection, statement, db, Map.empty, Map.empty, key )
		
		def traverseDefinitions( list: List[AST] ) = list foreach interpretDefinitions
		
		def interpretDefinitions( ast: AST ): Unit =
			ast match {
				case SourceAST( list ) => traverseDefinitions( list )
				case EnumDefinition( pos, name, enum ) =>
					if ((defines contains name) || (tables contains db.desensitize( name )))
						problem( pos, s"'$name' already defined" )

					val set = new HashSet[String]

					for ((p, e) <- enum)
						if (set(e))
							problem( p, s"'$e' is a duplicate" )
						else
							set += e

					defines(name) = new Enum( enum map {case (_, e) => e} )
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
				case TableDefinition( protection, pos, name, base, columns, resource ) =>
					var mtm = false
					var ma = false

					if (tables.contains( db.desensitize(name) ) || defines.contains( name ) && defines(name).isInstanceOf[Enum])
						problem( pos, s"'$name' already defined" )
					
					val cols = new LinkedHashMap[String, Column]
					
					columns foreach {
						case col@TableColumn( cname, typ, modifiers, validators ) =>
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

							typ match {
								case ArrayType( MediaType(_, _, _), _, _, _ ) => ma = true
								case _ =>
							}

							val typ1 =
								typ match {
									case t@IdentType( ident ) =>
										defines get ident match {
											case Some( e: Enum ) => EnumType( ident, e.enum.toVector )
											case _ =>
												val res = SingleReferenceType( ident, null )

												res.pos = t.pos
												res
										}
									case t => t
								}

							cols(cname) = Column( cname, typ1, secret, required, unique, indexed, validators )
					}

					tables(db.desensitize( name )) = Table( name, cols map {case (_, cinfo) => cinfo} toList, cols.toMap, resource, base, mtm, ma, null, null )

					if (resource) {
						val mtm = cols.values exists (c => c.typ.isInstanceOf[ManyReferenceType])
						val authorize =
							protection match {
								case None => ""
								case Some( None ) => "protected"
								case Some( Some(null) ) => "private"
								case Some( Some(g) ) => s"protected ($g)"
							}

						if (base isEmpty) {
							routes ++= configure_( Builtins.routes.replaceAll("<base>", "").replaceAll("<resource>", name).replaceAll("<authorize>", authorize),
								connection, statement, db ).routes

							if (mtm)
								routes ++= configure_( Builtins.mtmroutes.replaceAll("<base>", "").replaceAll("<resource>", name).replaceAll("<authorize>", authorize),
									connection, statement, db ).routes

							if (cols exists {case (_, c) => c.typ.isInstanceOf[ArrayType]})
								routes ++= configure_( Builtins.arrayroutes.replaceAll("<base>", "").replaceAll("<resource>", name).replaceAll("<authorize>", authorize),
									connection, statement, db ).routes
						} else
							for (URIPath( path ) <- base) {
								val basepath = path map {case NameURISegment( segment ) => segment} mkString("/", "/", "")

								routes ++= configure_( Builtins.routes.replaceAll("<resource>", name).
									replaceAll("<base>", basepath ).replaceAll("<authorize>", authorize),
									connection, statement, db ).routes

								if (mtm)
									routes ++= configure_( Builtins.mtmroutes.replaceAll("<resource>", name).
										replaceAll("<base>", basepath).replaceAll("<authorize>", authorize),
										connection, statement, db ).routes

								if (cols exists { case (_, c) => c.typ.isInstanceOf[ArrayType] })
									routes ++= configure_( Builtins.arrayroutes.replaceAll("<resource>", name).
										replaceAll("<base>", basepath).replaceAll("<authorize>", authorize),
										connection, statement, db ).routes
							}
					}
				case RoutesDefinition( URIPath(base), protection, mappings ) =>
					val block = new ArrayBuffer[Route]

					mappings foreach {
						case URIMapping( HTTPMethod(method), URIPath(path), action ) =>
							block += Route( method, URIPath(base ++ path), action )

							protection match {
								case None =>
								case Some( Some(null) ) =>
									for ((Route( method, path, action), i) <- block zipWithIndex)
										block(i) = Route( method, path,
											CompoundExpression(ApplyExpression(VariableExpression("access"), null,
												List(QueryParameterExpression("access_token"))), action) )
								case Some( pro ) =>
									for ((Route( method, path, action), i) <- block zipWithIndex)
										block(i) = Route( method, path,
											CompoundExpression(ApplyExpression(VariableExpression("authorize"), null,
												List(LiteralExpression(pro), QueryParameterExpression("access_token"))), action) )
							}
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
					case Some( Table(name, columns, columnMap, _, _, mtm, ma, _, _ )) =>
						tables(k) = Table(name, v.columns ++ columns, v.columnMap ++ columnMap, v.resource, v.base, v.mtm || mtm, ma, null, null )
					case None => tables(k) = v
				}

			tables.values foreach {
				case Table( _, columns, _, _, _, _, _, _, _ ) =>
					columns foreach {
						case Column( _, t@SingleReferenceType(table, _), _, _, _, _, _ ) =>
							t.ref = tables.getOrElse( db.desensitize(table), problem(t.pos, s"'$table' not found") )
						case Column( _, t@ManyReferenceType(table, _), _, _, _, _, _ ) =>
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
				case t@Table( name, cols, _, _, _, _, _, _, _ ) =>
					val cnames1 = cols filterNot (c => c.typ.isInstanceOf[ManyReferenceType]) map (c => c.name)
					val columns = cnames1 map nameIn mkString ","
					val values = Seq.fill( cnames1.length )( "?" ) mkString ","

					t.preparedInsert = connection.prepareStatement( s"INSERT INTO $name ($columns) VALUES ($values)" )
					t.preparedFullInsert = connection.prepareStatement( s"INSERT INTO $name ($idIn, $columns) VALUES (?, $values)" )
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

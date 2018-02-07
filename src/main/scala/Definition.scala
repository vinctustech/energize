package xyz.hyperreal.energize2

import java.sql._

import collection.mutable.{ArrayBuffer, HashMap}
import collection.JavaConverters._

import org.mindrot.jbcrypt.BCrypt

import xyz.hyperreal.bvm.{AST, Compilation, ExpressionAST, SourceAST}


object Definition {

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

	def define( src: String, connection: Connection, statement: Statement, database: Database, key: String ): (Processor, ArrayBuffer[Route]) =
		define( io.Source.fromString(src), connection, statement, database, key )

	def define( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ): (Processor, ArrayBuffer[Route]) = {
		val p = new EnergizeParser

		define( p.parseFromSource(src, p.source), connection, statement, database, key )
	}

//	def primitive( typ: JSON ): PrimitiveColumnType =
//		typ getString "type" match {
//			case "string" => StringType
//			case "text" => TextType
//			case "boolean" => BooleanType
//			case "integer" => IntegerType
//			case "long" => LongType
//			case "uuid" => UUIDType
//			case "date" => DateType
//			case "datetime" => DatetimeType
//			case "time" => TimeType
//			case "timestamp" => TimestampType
//			case "binary" => BinaryType
//			case "blob" => BLOBType( 'base64 )
//			case "float" => FloatType
//			case "decimal" =>
//				val List( prec, scale ) = typ.getList[Int]( "parameters" )
//
//				DecimalType( prec, scale )
////			case "enum" => EnumType( _, typ.getList[String]("parameters").toVector )
//			case "media" =>
//				val List( allowed, limit ) = typ.getList[AnyRef]( "parameters" )
//				val allowed1 =
//					for (a <- allowed.asInstanceOf[List[String]])
//						yield
//							a match {
//								case MIME( typ, subtype ) => MimeType( typ, subtype )
//								case _ => sys.error( s"bad MIME type: $a" )
//							}
//
//				if (limit eq null)
//					MediaType( allowed1, null, Int.MaxValue )
//				else
//					MediaType( allowed1, null, limit.asInstanceOf[Int] )
//		}
//
//	def parsePath( path: String ): Option[URIPath] =
//		if (path endsWith "/")
//			None
//		else
//			path split "/" toList match {
//				case Nil|List( "" ) => None
//				case a =>
//					if (a.head != "")
//						None
//					else {
//						val tail = a.tail
//
//						if (tail contains "")
//							None
//						else
//							Some( URIPath(tail map NameURISegment) )
//					}
//			}
//
//	def defineFromJSON( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ): Environment = {
//		val s = src mkString
//		val json = DefaultJSONReader.fromString( s )
//		val decl = new ListBuffer[StatementAST]
//
//		for ((k, v) <- json)
//			k match {
//				case "tables" =>
//					for (tab <- v.asInstanceOf[List[JSON]]) {
//						val pro =
//							tab get "protection" match {
//								case None => None
//								case Some( groups: List[_] ) => Some( groups.asInstanceOf[List[String]].headOption )
//								case Some( _ ) => sys.error( "protection expected to be a list" )
//							}
//						val priv =
//							tab get "private" match {
//								case None => false
//								case Some( _ ) => true
//							}
//						val base =
//							if (tab contains "base")
//								parsePath( tab getString "base" )
//							else
//								None
//						val cols = new ListBuffer[TableColumn]
//
//						for (c <- tab.getList[JSON]( "fields" )) {
//							val typ = c getMap "type"
//							val cat = typ getString "category"
//							val ctyp =
//								cat match {
//									case "primitive" => primitive( typ )
//									case "array" => ArrayType( primitive(typ), null, null, 1 )
//									case "one-to-many" => SingleReferenceType( typ getString "type", null )
//									case "many-to-many" => ManyReferenceType( typ getString "type", null )
//								}
//							val modifiers =
//								if (c contains "modifiers")
//									c getList[String] "modifiers" map ColumnTypeModifier
//								else
//									Nil
//							val validators =
//								if (c contains "validators")
//									c getList[String] "validators"
//								else
//									Nil
//
//							cols += TableColumn( c getString "name", ctyp, modifiers, validators map EnergizeParser.parseValidator )
//						}
//
//						decl += TableDefinition( pro, null, tab getString "name", base, cols toList, tab.getBoolean("resource") )
//					}
//				case "routes" =>
//					for (routes <- v.asInstanceOf[List[JSON]]) {
//						val base =
//							if (routes contains "base")
//								parsePath( routes getString "base" ).get
//							else
//								URIPath( Nil )
//						val protection =
//							if (routes contains "protection") {
//								val p = routes getString "protection"
//
//								if (p eq null)
//									Some( None )
//								else
//									Some( Some(p) )
//							} else
//								None
//						val mappings =
//							for (m <- routes.getList[JSON]( "mappings" ))
//								yield {
//									val method = HTTPMethod( m getString "method" )
//									val path = parsePath( m getString "path" ).get
//									val action =
//										m getString "language" match {
//											case "ESL" => parseExpression( m getString "action" )
//											case "ECMAScript" => JavaScriptExpression( m getString "action" )
//										}
//									URIMapping( method, path, action )
//								}
//
//						decl += RoutesDefinition( base, protection, mappings )
//					}
//			}
//
//		define( SourceAST(decl toList), connection, statement, database, key )
//	}

	def parse( src: String ): AST = parse( io.Source.fromString(src) )

	def parse( src: io.Source ) = {
		val p = new EnergizeParser

		p.parseFromSource( src, p.source )
	}

	def compile( src: String, db: Database, internal: Boolean ): Definition = compile( parse(src), db, internal )

	def compile( ast: AST, db: Database, internal: Boolean ) = {
		val compiler = new EnergizeCompiler
		val code = compiler.compile( ast, db, internal )

		Definition( code, compiler.resources, compiler.routes, compiler.conds )
	}

	def define( ast: SourceAST, connection: Connection, statement: Statement, db: Database, key: String ): (Processor, ArrayBuffer[Route]) = {
		val Definition( code, resources, routes, _ ) = compile( ast, db, false )

		resources.values foreach {
			case Resource( _, fields, _, _, _, _ ) =>
				fields foreach {
					case Field( _, t@SingleReferenceType(pos, table, _), _, _, _, _, _ ) =>
						t.ref = resources.getOrElse( db.desensitize(table), problem(pos, s"'$table' not found") )
					case Field( _, t@ManyReferenceType(pos, table, _), _, _, _, _, _ ) =>
						t.ref = resources.getOrElse( db.desensitize(table), problem(pos, s"'$table' not found") )
					case _ =>
				}
		}

		val sorted = ResourceSorter.sort( resources.values ) getOrElse sys.error( "resources cannot be topologically ordered" )

		if (resources.nonEmpty && !connection.getMetaData.getTables( null, db.publicSchema, resources.head._1, null ).next) {
			//			print( xyz.hyperreal.table.TextTable(connection.getMetaData.getTables( null, null, tables.head._1, null )) )
//				println( db.create(sorted) )
			statement.execute( db.create(sorted) )
		}

		resources.values foreach {
			case r@Resource( name, fields, _, _, _, _ ) =>
				val cnames1 = fields filterNot (_.typ.isInstanceOf[ManyReferenceType]) map (_.name)
				val fieldstr = cnames1 map nameIn mkString ","
				val values = Seq.fill( cnames1.length )( "?" ) mkString ","

				r.preparedInsert = connection.prepareStatement( s"INSERT INTO $name ($fieldstr) VALUES ($values)" )
				r.preparedFullInsert = connection.prepareStatement( s"INSERT INTO $name ($idIn, $fieldstr) VALUES (?, $values)" )
		}

		val proc = new Processor( code, connection, statement, db, resources.toMap, key )
		val admin =
			Map( ADMIN.entrySet.asScala.toList.map( e =>
				(e.getKey, ADMIN.getValue(e.getKey).unwrapped) match {
					case (k, o: java.util.List[_]) => (k, o.asScala)
					case ("password", p: String) => ("password", BCrypt.hashpw( p, BCrypt.gensalt ))
					case (k, o) => (k, o)
				}
			) :+ "createdTime" -> now: _* )

		CommandFunctions.insert( proc.vm, resources(db.desensitize("users")), admin )

		(proc, routes)
	}
}

case class Definition( code: Compilation, resources: HashMap[String, Resource], routes: ArrayBuffer[Route],
											 conds: ArrayBuffer[(ExpressionAST, ExpressionAST)] )
package xyz.hyperreal.energize2

import java.sql._

import collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import collection.JavaConverters._
import org.mindrot.jbcrypt.BCrypt
import xyz.hyperreal.bvm._
import xyz.hyperreal.json.{DefaultJSONReader, JSON}

import scala.util.parsing.input.Position


object Definition {

	val MIME = """([a-z+]+)/(\*|[a-z+]+)"""r

	def dbtype( pos: Position, name: String, args: List[Any], array: Boolean ) = {
		val args1 =
			args map {
				case a: String if a forall (_.isDigit) => a.toInt
				case a => a
			}

		val basic =
			name match {
				case "boolean" if args nonEmpty => problem( pos, "the 'boolean' type doesn't take parameters" )
				case "boolean" => BooleanType
				case "char" if args.isEmpty || args.length > 1 || !integer( args.head.toString ) =>
					problem( pos, "the 'char' type takes a positve integer parameter" )
				case "char" => CharType( args.head.toString.toInt )
				case "string" if args.length > 1 || args.nonEmpty && !integer( args.head.toString ) =>
					problem( pos, "the 'string' type takes a positve integer size parameter, or else the size defaults to 128" )
				case "string" => StringType( if (args nonEmpty) args.head.toString.toInt else 128 )
				case "tinytext" if args nonEmpty => problem( pos, "the 'tinytext' type doesn't take parameters" )
				case "tinytext" => TinytextType
				case "shorttext" if args nonEmpty => problem( pos, "the 'shorttext' type doesn't take parameters" )
				case "shorttext" => ShorttextType
				case "text" if args nonEmpty => problem( pos, "the 'text' type doesn't take parameters" )
				case "text" => TextType
				case "longtext" if args nonEmpty => problem( pos, "the 'longtext' type doesn't take parameters" )
				case "longtext" => LongtextType
				case "tinyint" if args nonEmpty => problem( pos, "the 'tinyint' type doesn't take parameters" )
				case "tinyint" => TinyintType
				case "smallint" if args nonEmpty => problem( pos, "the 'smallint' type doesn't take parameters" )
				case "smallint" => SmallintType
				case "integer"|"int" if args nonEmpty => problem( pos, "the 'integer' type doesn't take parameters" )
				case "integer"|"int" => IntegerType
				case "bigint" if args nonEmpty => problem( pos, "the 'bigint' type doesn't take parameters" )
				case "bigint" => BigintType
				case "UUID"|"uuid" if args nonEmpty => problem( pos, "the 'UUID' type doesn't take parameters" )
				case "UUID"|"uuid" => UUIDType
				case "date" if args nonEmpty => problem( pos, "the 'date' type doesn't take parameters" )
				case "date" => DateType
				case "datetime" if args nonEmpty => problem( pos, "the 'datetime' type doesn't take parameters" )
				case "datetime" => DatetimeType
				case "time" if args nonEmpty => problem( pos, "the 'time' type doesn't take parameters" )
				case "time" => TimeType
				case "timestamp" if args nonEmpty => problem( pos, "the 'timestamp' type doesn't take parameters" )
				case "timestamp" => TimestampType
				case "binary" if args nonEmpty => problem( pos, "the 'binary' type doesn't take parameters" )
				case "binary" => BinaryType
				case "float" if args nonEmpty => problem( pos, "the 'float' type doesn't take parameters" )
				case "float" => FloatType
				case "blob" if args.length > 1 || args.length == 1 && !(List("base64", "hex", "list", "urlchars") contains args.head.toString)  =>
					problem( pos, """the 'blob' type takes an option blob type parameter: "base64", "hex", "list", or "urlchars"""" )
				case "blob" if args nonEmpty => BLOBType( Symbol(args.head.toString) )
				case "blob" => BLOBType( 'base64 )
				case "media" if args exists (!_.isInstanceOf[MimeType]) =>
					problem( pos, "the 'media' type takes zero or more media (MIME) type parameters" )
				case "media" => MediaType( args.asInstanceOf[List[MimeType]] )
				case r =>	//todo: could be EnumType if previously defined as such
					if (array)
						ManyReferenceType( pos, r )
					else
						SingleReferenceType( pos, r )
			}

		if (array && !basic.isInstanceOf[ManyReferenceType])
			ArrayType( basic.asInstanceOf[PrimitiveFieldType] )
		else
			basic
	}

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

//	def primitive( typ: JSON ): PrimitiveFieldType =
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

	def parsePath( path: String ): Option[PathSegment] =
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
							Some( if (tail.length == 1) LiteralPathSegment(tail.head) else ConcatenationPathSegment(tail map LiteralPathSegment) )
					}
			}

//	def defineFromJSON( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ): Processor = {
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
//						val cols = new ListBuffer[ResourceField]
//
//						for (c <- tab.getList[JSON]( "fields" )) {
//							val typ = c getMap "type"
//							val cat = typ getString "category"
//							val ctyp =
//								cat match {
//									case "primitive" => Definition.dbtype( null, typ getString "type", typ getList[AnyRef] "parameters", false )
//									case "array" => Definition.dbtype( null, typ getString "type", typ getList[AnyRef] "parameters", true )
//									case "one-to-many" => SingleReferenceType( null, typ getString "type", null )
//									case "many-to-many" => ManyReferenceType( null, typ getString "type", null )
//								}
//							val modifiers =
//								if (c contains "modifiers")
//									c getList[String] "modifiers" map (m => (null, m))
//								else
//									Nil
//							val validators =
//								if (c contains "validators")
//									c getList[String] "validators"
//								else
//									Nil
//
//							cols += ResourceField( c getString "name", ctyp, modifiers, false )
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
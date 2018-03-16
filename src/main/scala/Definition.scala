//@
package xyz.hyperreal.energize

import java.sql.{Connection, Statement, DriverManager}

import collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import collection.JavaConverters._

import org.mindrot.jbcrypt.BCrypt

import xyz.hyperreal.bvm._
import xyz.hyperreal.json.{DefaultJSONReader, JSON}


object Definition {

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

	def define( src: String, connection: Connection, statement: Statement, database: Database, key: String ): Processor =
		define( io.Source.fromString(src), connection, statement, database, key )

	def define( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ): Processor = {
		val p = new EnergizeParser

		define( p.parseFromSource(src, p.source), connection, statement, database, key )
	}

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

	def defineFromJSON( src: io.Source, connection: Connection, statement: Statement, database: Database, key: String ) = {
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
//						val priv =	//todo: implement 'private' in defineFromJSON()
//							tab get "private" match {
//								case None => false
//								case Some( _ ) => true
//							}
						val base =
							if (tab contains "base")
								parsePath( tab getString "base" )
							else
								None
						val cols = new ListBuffer[ResourceField]

						for (c <- tab.getList[JSON]( "fields" )) {
							val typ = c getMap "type"
							val cat = typ getString "category"
							val array =
								cat match {
									case "primitive" => false
									case "array" => true
									case "one-to-many" => false
									case "many-to-many" => true
								}
							val modifiers =
								if (c contains "modifiers")
									c getList[String] "modifiers" map (m => (null, m))
								else
									Nil
//							val validators =	//todo: validators in defineFromJSON()
//								if (c contains "validators")
//									c getList[String] "validators"
//								else
//									Nil
							val args =
								if (typ contains "parameters")
									typ getList[AnyRef] "parameters"
								else
									Nil

							cols += ResourceField( null, c getString "name", null, typ getString "type", args, modifiers, array )
						}

						decl += ResourceDefinition( pro, null, tab getString "name", base, cols toList, tab getBoolean "resource" )
					}
				case "routes" =>
					for (routes <- v.asInstanceOf[List[JSON]]) {
						val base =
							if (routes contains "base")
								parsePath( routes getString "base" )
							else
								None
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
									val method = m getString "method"
									val path = parsePath( m getString "path" ).get
									val action =
										m getString "language" match {
											case "ESL" => parseExpression( m getString "action" )
											//case "ECMAScript" => JavaScriptExpression( m getString "action" )
										}
									RouteMapping( null, method, path, None, action )	//todo: add route guards to JSON config
								}

						decl += RoutesDefinition( null, base, protection, mappings )
					}
			}

		define( SourceAST(decl toList), connection, statement, database, key )
	}

	def parse( src: String ): SourceAST = parse( io.Source.fromString(src) )

	def parse( src: io.Source ) = {
		val p = new EnergizeParser

		p.parseFromSource( src, p.source )
	}

	def compile( src: String, connection: Connection, db: Database, stat: Statement, internal: Boolean ): Definition = compile( parse(src), connection, db, stat, internal )

	def compile( ast: SourceAST, connection: Connection, db: Database, stat: Statement, internal: Boolean ) = {
		val compiler = new EnergizeCompiler
		val code = compiler.compile( ast, connection, db, stat, internal )

		Definition( code, compiler.resources, compiler.routes, compiler.conds )
	}

	def define( ast: SourceAST, connection: Connection, statement: Statement, db: Database, key: String ): Processor = {
		val Definition( code, resources, routes, _ ) = compile( ast, connection, db, statement, false )

		resources.values foreach {
			case Resource( _, _, fields, _, _, _, _ ) =>
				fields foreach {
					case Field( _, t@SingleReferenceType(pos, table, _), _, _, _, _, _ ) =>
						t.ref = resources.getOrElse( db.desensitize(table), problem(pos, s"'$table' not found") )
					case Field( _, t@ManyReferenceType(pos, table, _), _, _, _, _, _ ) =>
						t.ref = resources.getOrElse( db.desensitize(table), problem(pos, s"'$table' not found") )
					case _ =>
				}
		}

		val sorted = ResourceSorter.sort( resources.values ) getOrElse sys.error( "resources cannot be topologically ordered" )
    val created = db.created( connection, resources )

		if (resources.nonEmpty && !created) {
			//			print( xyz.hyperreal.table.TextTable(connection.getMetaData.getTables( null, null, tables.head._1, null )) )
			//println( db.create(sorted) )
			statement.execute( db.create(sorted) )
		}

		val media = resources( db.desensitize("_media_") )
		val users = resources( db.desensitize("users") )
		val proc = new Processor( code, connection, statement, db, resources.toMap, routes.toList, key, users )

		resources.values foreach {
			case r@Resource( name, _, fields, _, _, _, _ ) =>
				val cnames1 = fields filterNot (_.typ.isInstanceOf[ManyReferenceType]) map (_.name)
				val fieldstr = cnames1 map nameIn mkString ","
				val values = Seq.fill( cnames1.length )( "?" ) mkString ","

				r.preparedInsert =
          connection.prepareStatement(
            s"INSERT INTO $name ($fieldstr) VALUES ($values)" + (if (db == PostgresDatabase) " RETURNING _id" else "") )
				r.preparedFullInsert = connection.prepareStatement( s"INSERT INTO $name ($idIn, $fieldstr) VALUES (?, $values)" )
				r.media = media
				r.processor = proc
		}

		proc.vm.execute

    if (!created) {
      val admin =
        Map( ADMIN.entrySet.asScala.toList.map( e =>
          (e.getKey, ADMIN.getValue(e.getKey).unwrapped) match {
            case (k, o: java.util.List[_]) => (k, o.asScala)
            case ("password", p: String) => ("password", BCrypt.hashpw( p, BCrypt.gensalt ))
            case (k, o) => (k, o)
          }
        ) :+ "createdTime" -> now: _* )

      users.insert( admin )
    }

		proc
	}
}

case class Definition( code: Compilation, resources: HashMap[String, Resource], routes: ArrayBuffer[Route],
											 conds: ArrayBuffer[(ExpressionAST, ExpressionAST)] )
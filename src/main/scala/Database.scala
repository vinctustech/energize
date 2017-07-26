package xyz.hyperreal.energize

import java.sql.Timestamp
import java.time.format.DateTimeFormatter
import java.time.{OffsetDateTime, Instant, ZoneOffset, ZoneId}


object H2Database extends Database {
	val caseSensitive = false
	val publicSchema = "PUBLIC"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd HH:mm:ss.SSS" )
	val ZONEID = zoneId
	val SYSTEM_ZONEID = ZoneId.systemDefault

	def readTimestamp( d: String ): Timestamp = {
		val t = OffsetDateTime.parse( d )
		val u = t.toInstant.atZone( SYSTEM_ZONEID ).toLocalDateTime//atOffset( ZoneOffset.UTC )

//		u.format( TIMESTAMP_FORMAT )
		Timestamp.valueOf( u )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def desensitize( name: String ) = name.toUpperCase

	def conflict( error: String ) = error startsWith "Unique index or primary key violation"

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= s" ($idIn IDENTITY NOT NULL PRIMARY KEY"

				columns foreach {
					case Column( cname, typ, secret, required, unique, indexed ) =>

					if (!typ.isInstanceOf[ManyReferenceType]) {
						buf ++= ", "
						buf ++= nameIn( cname )
						buf += ' '
						parameters( typ )
						buf ++=
							(typ match {
								case EnumType( _, enum ) => "ENUM(" + enum.map( "'" + _ + "'" ).mkString( "," ) + ")"
								case StringType => "VARCHAR(255)"
								case TextType => "TEXT"
								case BooleanType => "BOOLEAN"
								case IntegerType => "INT"
								case LongType => "BIGINT"
								case FloatType => "FLOAT"
								case UUIDType => "UUID"
								case DateType => "DATE"
								case DatetimeType => "DATETIME"
								case TimeType => "TIME"
								case TimestampType => "TIMESTAMP"
								case TimestamptzType => "TIMESTAMP WITH TIMEZONE"
								case BinaryType => "BINARY"
								case DecimalType( prec, scale ) => s"DECIMAL($prec,$scale)"
								case SingleReferenceType( _, _ ) => "BIGINT"
								case ArrayType( _, _, _, _ ) => "ARRAY"
								case BLOBType( _ ) => "BLOB"
								case MediaType( _, _, _ ) => "BIGINT"
							})

						if (required)
							buf ++= " NOT NULL"

						if (unique)
							buf ++= " UNIQUE"
					}
				}

				columns foreach {
					case Column( fk, SingleReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY (${nameIn(fk)}) REFERENCES $ref ($idIn) ON DELETE CASCADE"
					case Column( fk, MediaType(_, _, _), _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY (${nameIn(fk)}) REFERENCES _media_ ($idIn) ON DELETE CASCADE"
					case _ =>
				}

				buf ++= ");\n"

				columns foreach {
					case Column( c, _, _, _, _, true ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= nameIn( c )
						buf ++= ");\n"
					case Column( _, ManyReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name ($idIn) ON DELETE CASCADE, "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref ($idIn) ON DELETE CASCADE, "
						buf ++= s"PRIMARY KEY ($name$$id, $ref$$id));\n"
					case _ =>
				}
		}

		buf.toString
	}
}

object PostgresDatabase extends Database {
	val publicSchema = "public"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )
	val ZONEID = zoneId

	def readTimestamp( d: String ) = {
		val t = OffsetDateTime.parse( d )
		val u = t.toInstant.atOffset( ZoneOffset.UTC ).toLocalDateTime

//		u.format( TIMESTAMP_FORMAT )
		Timestamp.valueOf( u )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def primitive( typ: PrimitiveColumnType ) =
		typ match {
			case StringType => "VARCHAR(255)"
			case TextType => "TEXT"
			case BooleanType => "BOOLEAN"
			case IntegerType => "INT"
			case LongType => "BIGINT"
			case FloatType => "FLOAT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "TIMESTAMP"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case TimestamptzType => "TIMESTAMP WITH TIME ZONE"
			case BLOBType( _ ) => "BLOB"
			case MediaType( _, _, _ ) => "BIGINT"
		}

	def desensitize( name: String ) = name.toLowerCase

	def conflict( error: String ) = sys.error( "not done yet" )

	def create( tables: List[Table] ) = {
		val enums = new StringBuilder
		val buf = new StringBuilder

		tables foreach {
			case Table( name, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (_id BIGSERIAL NOT NULL PRIMARY KEY"

				columns foreach {
					case Column( cname, typ, secret, required, unique, indexed ) =>

					buf ++= ", "
					buf ++= cname
					buf += ' '
					parameters( typ )
					buf ++=
						(typ match {
							case EnumType( ename, enum ) =>
								enums ++= s"CREATE TYPE $ename AS ENUM ("
								enums ++= enum map (i => s"'$i'") mkString ","
								enums ++= ");"
								ename
							case p: PrimitiveColumnType => primitive( p )
							case SingleReferenceType( _, _ ) => "BIGINT"
							case ArrayType( t, _, _, d ) => primitive( t ) + "[]"( d )
						})

					if (required)
						buf ++= " NOT NULL"

					if (unique)
						buf ++= " UNIQUE"
				}

				columns foreach {
					case Column( fk, SingleReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY ($fk) REFERENCES $ref (_id) ON DELETE CASCADE"
					case Column( fk, MediaType(_, _, _), _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY ($fk) REFERENCES _media_ (_id) ON DELETE CASCADE"
					case _ =>
				}

				buf ++= ");\n"

				columns foreach {
					case Column( c, _, _, _, _, true ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= c
						buf ++= ");\n"
					case Column( _, ManyReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name (_id) ON DELETE CASCADE, "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref (_id) ON DELETE CASCADE, "
						buf ++= s"PRIMARY KEY ($name$$id, $ref$$id));\n"
					case _ =>
				}
		}

		enums.toString + buf.toString
	}
}

object MySQLDatabase extends Database {
	val publicSchema = "public"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )
	val ZONEID = zoneId

	def readTimestamp( d: String ) = {
		val t = OffsetDateTime.parse( d )
		val u = t.toInstant.atOffset( ZoneOffset.UTC ).toLocalDateTime

//		u.format( TIMESTAMP_FORMAT )
		Timestamp.valueOf( u )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def primitive( typ: PrimitiveColumnType ) =
		typ match {
			case StringType => "VARCHAR(255)"
			case TextType => "LONGTEXT"
			case BooleanType => "BOOLEAN"
			case IntegerType => "INT"
			case LongType => "BIGINT"
			case FloatType => "FLOAT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "DATETIME"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case TimestamptzType => problem( typ.pos, "'with timezone' is not supported by MySQL" )
		}

	def desensitize( name: String ) = name

	def conflict( error: String ) = sys.error( "not done yet" )

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (_id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY"

				columns foreach {
					case Column( cname, typ, secret, required, unique, indexed ) =>

					buf ++= ", "
					buf ++= cname
					buf += ' '
					buf ++=
						(typ match {
							case p: PrimitiveColumnType => primitive( p )
							case SingleReferenceType( _, _ ) => "BIGINT"
						})

					if (required)
						buf ++= " NOT NULL"

					if (unique)
						buf ++= " UNIQUE"
				}

				columns foreach {
					case Column( fk, SingleReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY ($fk) REFERENCES $ref (_id) ON DELETE CASCADE"
					case Column( fk, MediaType(_, _, _), _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY ($fk) REFERENCES _media_ (_id) ON DELETE CASCADE"
					case _ =>
				}

				buf ++= ");\n"

				columns foreach {
					case Column( c, _, _, _, _, true ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= c
						buf ++= ");\n"
					case Column( _, ManyReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name (_id) ON DELETE CASCADE, "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref (_id) ON DELETE CASCADE, "
						buf ++= s"PRIMARY KEY ($name$$id, $ref$$id));\n"
					case _ =>
				}
		}

		buf.toString
	}
}

object Database {
	private val supported = Map[String, Database](
		"H2" -> H2Database,
		"PostgreSQL" -> PostgresDatabase,
		"MySQL" -> MySQLDatabase
	)
	def isSupported( name: String ) = supported contains name

	def apply( name: String ) = supported( name )
}

abstract class Database {
	val publicSchema: String

	def zoneId =
		DATETIME.getString( "timezone" ) match {
			case "system" => ZoneId.systemDefault
			case z => ZoneId.of( z )
		}

	def desensitize( name: String ): String

	protected def parameters( typ: ColumnType ) {
		typ match {
			case ArrayType( _, _, null, _ ) =>
			case a@ArrayType( _, p, d, _ ) =>
				if (d matches "[0-9]+") {
					val v = BigInt( d )

					if (!v.isValidInt || v < 1)
						problem( p, "dimension out of range" )

					a.dimint = v.toInt
				} else
					problem( p, "dimension must be an integer" )
			case _ =>
		}
	}

	def readTimestamp( d: String ): Timestamp

	def writeTimestamp( o: Any ): String

	def conflict( error: String ): Boolean

	def create( tables: List[Table] ): String
}
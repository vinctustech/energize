package xyz.hyperreal.energize

import java.sql.Timestamp
import java.time.format.DateTimeFormatter
import java.time.{OffsetDateTime, Instant, ZoneOffset, ZoneId}


object H2Database extends Database {
	val caseSensitive = false
	val publicSchema = "PUBLIC"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )
	val ZONEID = zoneId
	val SYSTEM_ZONEID = ZoneId.systemDefault

	def readTimestamp( d: String ) = {
		val t = OffsetDateTime.parse( d )
		val u = t.toInstant.atZone( SYSTEM_ZONEID ).toLocalDateTime//atOffset( ZoneOffset.UTC )

		u.format( TIMESTAMP_FORMAT )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def desensitize( name: String ) = name.toUpperCase

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (id IDENTITY NOT NULL PRIMARY KEY"

				columns foreach {
					case Column( cname, typ, secret, required, unique, indexed ) =>

					if (!typ.isInstanceOf[ManyReferenceType]) {
						buf ++= ", "
						buf ++= cname
						buf += ' '
						dimension( typ )
						buf ++=
							(typ match {
								case StringType => "VARCHAR(255)"
								case IntegerType => "INT"
								case LongType => "BIGINT"
								case UUIDType => "UUID"
								case DateType => "DATE"
								case DatetimeType => "DATETIME"
								case TimeType => "TIME"
								case TimestampType => "TIMESTAMP"
								case TimestamptzType => "TIMESTAMP WITH TIMEZONE"
								case SingleReferenceType( _, _ ) => "BIGINT"
								case ArrayType( _, _, _, _ ) => "ARRAY"
							})

						if (required)
							buf ++= " NOT NULL"

						if (unique)
							buf ++= " UNIQUE"
					}
				}

				columns foreach {
					case Column( fk, SingleReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= ", FOREIGN KEY ("
						buf ++= fk
						buf ++= ") REFERENCES "
						buf ++= ref
						buf ++= "(id)"
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
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name (id), "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref (id), "
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

		u.format( TIMESTAMP_FORMAT )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def primitive( typ: PrimitiveColumnType ) =
		typ match {
			case StringType => "VARCHAR(255)"
			case IntegerType => "INT"
			case LongType => "BIGINT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "TIMESTAMP"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case TimestamptzType => "TIMESTAMP WITH TIME ZONE"
		}

	def desensitize( name: String ) = name.toLowerCase

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (id BIGSERIAL NOT NULL PRIMARY KEY"

				columns foreach {
					case Column( cname, typ, secret, required, unique, indexed ) =>

					buf ++= ", "
					buf ++= cname
					buf += ' '
					dimension( typ )
					buf ++=
						(typ match {
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
						buf ++= ", FOREIGN KEY ("
						buf ++= fk
						buf ++= ") REFERENCES "
						buf ++= ref
						buf ++= "(id)"
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
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name (id), "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref (id), "
						buf ++= s"PRIMARY KEY ($name$$id, $ref$$id));\n"
					case _ =>
				}
		}

		buf.toString
	}
}

object MySQLDatabase extends Database {
	val publicSchema = "public"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )
	val ZONEID = zoneId

	def readTimestamp( d: String ) = {
		val t = OffsetDateTime.parse( d )
		val u = t.toInstant.atOffset( ZoneOffset.UTC ).toLocalDateTime

		u.format( TIMESTAMP_FORMAT )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def primitive( typ: PrimitiveColumnType ) =
		typ match {
			case StringType => "VARCHAR(255)"
			case IntegerType => "INT"
			case LongType => "BIGINT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "DATETIME"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case TimestamptzType => problem( typ.pos, "'with timezone' is not supported by MySQL" )
		}

	def desensitize( name: String ) = name

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY"

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
						buf ++= ", FOREIGN KEY ("
						buf ++= fk
						buf ++= ") REFERENCES "
						buf ++= ref
						buf ++= "(id)"
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
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name (id), "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref (id), "
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

	def dimension( typ: ColumnType ) =
		typ match {
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

	def readTimestamp( d: String ): String

	def writeTimestamp( o: Any ): String

	def create( tables: List[Table] ): String
}
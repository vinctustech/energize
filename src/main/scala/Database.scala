package xyz.hyperreal.energize

import java.sql._
import java.time.format.DateTimeFormatter
import java.time.OffsetDateTime


object H2Database extends Database {
	val caseSensitive = false
	val publicSchema = "PUBLIC"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )

	def datetime( d: String ) = TIMESTAMP_FORMAT.format( OffsetDateTime.parse(d) )

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
	val caseSensitive = true
	val publicSchema = "public"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )

	def datetime( d: String ) = TIMESTAMP_FORMAT.format( OffsetDateTime.parse(d) )

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
	val caseSensitive = true
	val publicSchema = "public"
	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )

	def datetime( d: String ) = TIMESTAMP_FORMAT.format( OffsetDateTime.parse(d) )

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
	val caseSensitive: Boolean

	val publicSchema: String

	def desensitize( name: String ) =
		if (caseSensitive)
			name
		else
			name.toUpperCase

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

	def datetime( d: String ): String

	def create( tables: List[Table] ): String
}
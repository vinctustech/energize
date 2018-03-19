//@
package xyz.hyperreal.energize

import java.sql.{Connection, Timestamp}
import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneId, ZoneOffset}

import scala.collection.mutable


object H2Database extends Database {
	val caseSensitive = false
	val publicSchema = "PUBLIC"
	val arrayOpen = "("
	val arrayClose = ")"
//	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd HH:mm:ss.SSS" )
	val ZONEID = zoneId

	def created( connection: Connection, resources: mutable.HashMap[String, Resource] ) = connection.getMetaData.getTables( null, publicSchema, resources.head._1, null ).next

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

	def array( typ: PrimitiveFieldType ) =
		typ match {
			case CharType( length ) => s"CHAR($length)"
			case StringType( length ) => s"VARCHAR($length)"
			case TinytextType|ShorttextType|TextType|LongtextType => "TEXT"
			case BooleanType => "BOOLEAN"
			case TinyintType => "TINYINT"
			case SmallintType => "SMALLINT"
			case IntegerType => "INTEGER"
			case BigintType => "BIGINT"
			case FloatType => "FLOAT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "DATETIME"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case BinaryType => "BINARY"
			case DecimalType( prec, scale ) => s"DECIMAL($prec,$scale)"
			case BLOBType( _ ) => "BLOB"
			case MediaType( _ ) => "BIGINT"
		}

	def primitive( typ: PrimitiveFieldType ) =
		typ match {
			case CharType( length ) => s"CHAR($length)"
			case StringType( length ) => s"VARCHAR($length)"
			case TinytextType|ShorttextType|TextType|LongtextType => "TEXT"
			case BooleanType => "BOOLEAN"
			case TinyintType => "TINYINT"
			case SmallintType => "SMALLINT"
			case IntegerType => "INTEGER"
			case BigintType => "BIGINT"
			case FloatType => "FLOAT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "DATETIME"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case BinaryType => "BINARY"
			case DecimalType( prec, scale ) => s"DECIMAL($prec,$scale)"
			case BLOBType( _ ) => "BLOB"
			case MediaType( _ ) => "BIGINT"
		}

	def create( tables: List[Resource] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Resource( name, _, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= s" ($idIn IDENTITY NOT NULL PRIMARY KEY"

				columns foreach {
					case Field( cname, typ, secret, required, unique, indexed, _ ) =>

					if (!typ.isInstanceOf[ManyReferenceType]) {
						buf ++= ", "
						buf ++= nameIn( cname )
						buf += ' '
						parameters( typ )
						buf ++=
							(typ match {
								case EnumType( _, enum ) => "ENUM(" + enum.map( "'" + _ + "'" ).mkString( "," ) + ")"
								case p: PrimitiveFieldType => primitive( p )
								case SingleReferenceType( _, _, _ ) => "BIGINT"
								case ArrayType( _ ) => "ARRAY"
							})

						if (required)
							buf ++= " NOT NULL"

						if (unique)
							buf ++= " UNIQUE"
					}
				}

				columns foreach {
					case Field( fk, SingleReferenceType(_, ref, _), _, _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY (${nameIn(fk)}) REFERENCES $ref ($idIn) ON DELETE CASCADE"
					case Field( fk, MediaType(_), _, _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY (${nameIn(fk)}) REFERENCES _media_ ($idIn) ON DELETE CASCADE"
					case _ =>
				}

				buf ++= ");\n"

				columns foreach {
					case Field( c, _, _, _, _, true, _ ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= nameIn( c )
						buf ++= ");\n"
					case Field( _, ManyReferenceType(_, ref, _), _, _, _, _, _ ) =>
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
	val arrayOpen = "'{"
	val arrayClose = "}'"
//	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )
	val ZONEID = zoneId

	def created( connection: Connection, resources: mutable.HashMap[String, Resource] ) = connection.getMetaData.getTables( null, publicSchema, resources.head._1, null ).next

	def readTimestamp( d: String ) = {
		val t = OffsetDateTime.parse( d )
		val u = t.toInstant.atZone( SYSTEM_ZONEID ).toLocalDateTime//atOffset( ZoneOffset.UTC )
//		val u = t.toInstant.atOffset( ZoneOffset.UTC ).toLocalDateTime

//		u.format( TIMESTAMP_FORMAT )
		Timestamp.valueOf( u )
	}

	def writeTimestamp( o: Any ) = {
		val m = o.asInstanceOf[Timestamp].getTime
		val t = Instant.ofEpochMilli( m )

		t.atZone( ZONEID ).toOffsetDateTime.toString
	}

	def array( typ: PrimitiveFieldType ) =
		typ match {
			case CharType( length ) => s"CHAR($length)"
			case StringType( length ) => s"varchar"
			case TinytextType|ShorttextType|TextType|LongtextType => "TEXT"
			case BooleanType => "BOOLEAN"
			case TinyintType => "TINYINT"
			case SmallintType => "SMALLINT"
			case IntegerType => "INTEGER"
			case BigintType => "BIGINT"
			case FloatType => "FLOAT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "TIMESTAMP"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case DecimalType( prec, scale ) => s"DECIMAL($prec,$scale)"
			case BinaryType => "BYTEA"
			case BLOBType( _ ) => "BYTEA"
			case MediaType( _ ) => "BIGINT"
		}

	def primitive( typ: PrimitiveFieldType ) =
		typ match {
			case CharType( length ) => s"CHAR($length)"
			case StringType( length ) => s"VARCHAR($length)"
			case TinytextType|ShorttextType|TextType|LongtextType => "TEXT"
			case BooleanType => "BOOLEAN"
			case TinyintType => "TINYINT"
			case SmallintType => "SMALLINT"
			case IntegerType => "INTEGER"
			case BigintType => "BIGINT"
			case FloatType => "FLOAT"
			case UUIDType => "UUID"
			case DateType => "DATE"
			case DatetimeType => "TIMESTAMP"
			case TimeType => "TIME"
			case TimestampType => "TIMESTAMP"
			case DecimalType( prec, scale ) => s"DECIMAL($prec,$scale)"
			case BinaryType => "BYTEA"
			case BLOBType( _ ) => "BYTEA"
			case MediaType( _ ) => "BIGINT"
		}

	def desensitize( name: String ) = name.toLowerCase

	def conflict( error: String ) = error startsWith "ERROR: duplicate key value violates unique constraint"

	def create( tables: List[Resource] ) = {
		val enums = new StringBuilder
		val buf = new StringBuilder

		tables foreach {
			case Resource( name, _, columns, _, _, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= s" ($idIn BIGSERIAL NOT NULL PRIMARY KEY"

				columns foreach {
					case Field( cname, typ, secret, required, unique, indexed, _ ) =>

					if (!typ.isInstanceOf[ManyReferenceType]) {
						buf ++= ", "
						buf ++= nameIn( cname )
						buf += ' '
						parameters( typ )
						buf ++=
							(typ match {
								case EnumType( ename, enum ) =>
									enums ++= s"CREATE TYPE $ename AS ENUM ("
									enums ++= enum map (i => s"'$i'") mkString ","
									enums ++= ");"
									ename
								case p: PrimitiveFieldType => primitive( p )
								case SingleReferenceType( _, _, _ ) => "BIGINT"
								case ArrayType( t ) => primitive( t ) + "[]"
							})

						if (required)
							buf ++= " NOT NULL"

						if (unique)
							buf ++= " UNIQUE"
					}
				}

				columns foreach {
					case Field( fk, SingleReferenceType(_, ref, _), _, _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY (${nameIn(fk)}) REFERENCES $ref ($idIn) ON DELETE CASCADE"
					case Field( fk, MediaType(_), _, _, _, _, _ ) =>
						buf ++= s", FOREIGN KEY (${nameIn(fk)}) REFERENCES _media_ ($idIn) ON DELETE CASCADE"
					case _ =>
				}

				buf ++= ");\n"

				columns foreach {
					case Field( c, _, _, _, _, true, _ ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= nameIn( c )
						buf ++= ");\n"
					case Field( _, ManyReferenceType(_, ref, _), _, _, _, _, _ ) =>
						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name ($idIn) ON DELETE CASCADE, "
						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref ($idIn) ON DELETE CASCADE, "
						buf ++= s"PRIMARY KEY ($name$$id, $ref$$id));\n"
					case _ =>
				}
		}

		enums.toString + buf.toString
	}
}

//object MySQLDatabase extends Database {
//	val publicSchema = "public"
//	val TIMESTAMP_FORMAT =  DateTimeFormatter.ofPattern( "yyyy-MM-dd kk:mm:ss.SSS" )
//	val ZONEID = zoneId
//
//	def readTimestamp( d: String ) = {
//		val t = OffsetDateTime.parse( d )
//		val u = t.toInstant.atOffset( ZoneOffset.UTC ).toLocalDateTime
//
////		u.format( TIMESTAMP_FORMAT )
//		Timestamp.valueOf( u )
//	}
//
//	def writeTimestamp( o: Any ) = {
//		val m = o.asInstanceOf[Timestamp].getTime
//		val t = Instant.ofEpochMilli( m )
//
//		t.atZone( ZONEID ).toOffsetDateTime.toString
//	}
//
//	def primitive( typ: PrimitiveFieldType ) =
//		typ match {
//			case CharType( length ) => s"CHAR($length)"
//			case StringType( length ) => s"VARCHAR($length)"
//			case TinytextType => "TINYTEXT"
//			case ShorttextType => "TEXT"
//			case TextType => "MEDIUMTEXT"
//			case LongtextType => "LONGTEXT"
//			case BooleanType => "BOOLEAN"
//			case TinyintType => "TINYINT"
//			case SmallintType => "SMALLINT"
//			case IntegerType => "INTEGER"
//			case BigintType => "BIGINT"
//			case FloatType => "FLOAT"
//			case UUIDType => "UUID"
//			case DateType => "DATE"
//			case DatetimeType => "DATETIME"
//			case TimeType => "TIME"
//			case TimestampType => "TIMESTAMP"
//		}
//
//	def desensitize( name: String ) = name
//
//	def conflict( error: String ) = sys.error( "not done yet" )
//
//	def create( tables: List[Resource] ) = {
//		val buf = new StringBuilder
//
//		tables foreach {
//			case Resource( name, _, columns, _, _, _, _, _, _, _ ) =>
//				buf ++= "CREATE TABLE "
//				buf ++= name
//				buf ++= " (_id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY"
//
//				columns foreach {
//					case Field( cname, typ, secret, required, unique, indexed, _ ) =>
//
//					buf ++= ", "
//					buf ++= cname
//					buf += ' '
//					buf ++=
//						(typ match {
//							case p: PrimitiveFieldType => primitive( p )
//							case SingleReferenceType( _, _, _ ) => "BIGINT"
//						})
//
//					if (required)
//						buf ++= " NOT NULL"
//
//					if (unique)
//						buf ++= " UNIQUE"
//				}
//
//				columns foreach {
//					case Field( fk, SingleReferenceType(_, ref, _), _, _, _, _, _ ) =>
//						buf ++= s", FOREIGN KEY ($fk) REFERENCES $ref (_id) ON DELETE CASCADE"
//					case Field( fk, MediaType(_), _, _, _, _, _ ) =>
//						buf ++= s", FOREIGN KEY ($fk) REFERENCES _media_ (_id) ON DELETE CASCADE"
//					case _ =>
//				}
//
//				buf ++= ");\n"
//
//				columns foreach {
//					case Field( c, _, _, _, _, true, _ ) =>
//						buf ++= "CREATE INDEX ON "
//						buf ++= name
//						buf += '('
//						buf ++= c
//						buf ++= ");\n"
//					case Field( _, ManyReferenceType(_, ref, _), _, _, _, _, _ ) =>
//						buf ++= s"CREATE TABLE $name$$$ref ($name$$id BIGINT, FOREIGN KEY ($name$$id) REFERENCES $name (_id) ON DELETE CASCADE, "
//						buf ++= s"$ref$$id BIGINT, FOREIGN KEY ($ref$$id) REFERENCES $ref (_id) ON DELETE CASCADE, "
//						buf ++= s"PRIMARY KEY ($name$$id, $ref$$id));\n"
//					case _ =>
//				}
//		}
//
//		buf.toString
//	}
//}

object Database {
	private val supported = Map[String, Database](
		"H2" -> H2Database,
		"PostgreSQL" -> PostgresDatabase
//		"MySQL" -> MySQLDatabase
	)
	def isSupported( name: String ) = supported contains name

	def apply( name: String ) = supported( name )
}

abstract class Database {
	val SYSTEM_ZONEID = ZoneId.systemDefault
	val publicSchema: String

	def zoneId =
		SERVER.getString( "timezone" ) match {
			case "system" => ZoneId.systemDefault
			case z => ZoneId.of( z )
		}

	def desensitize( name: String ): String

	protected def parameters( typ: FieldType ) {
//		typ match {
//			case ArrayType( _, _, null, _ ) =>
//			case a@ArrayType( _, p, d, _ ) =>
//				if (d matches "[0-9]+") {
//					val v = BigInt( d )
//
//					if (!v.isValidInt || v < 1)
//						problem( p, "dimension out of range" )
//
//					a.dimint = v.toInt
//				} else
//					problem( p, "dimension must be an integer" )
//			case _ =>
//		}
	}

	val arrayOpen: String

	val arrayClose: String

	def readTimestamp( d: String ): Timestamp

	def writeTimestamp( o: Any ): String

	def conflict( error: String ): Boolean

	def create( tables: List[Resource] ): String

	def primitive( typ: PrimitiveFieldType ): String

	def array( typ: PrimitiveFieldType ): String

	def created( connection: Connection, resources: mutable.HashMap[String, Resource] ): Boolean
}
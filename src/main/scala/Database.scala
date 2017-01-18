package xyz.hyperreal.cras

import scala.collection.mutable.ListBuffer


object H2Database extends Database {
	val caseSensitive = false

	val publicSchema = "PUBLIC"

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, names, columns, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (id IDENTITY NOT NULL PRIMARY KEY"
				
				for (cname <- names) {
					val Column( _, typ, secret, required, unique, indexed ) = columns(desensitize(cname))

					if (!typ.isInstanceOf[ArrayReferenceType]) {
						buf ++= ", "
						buf ++= cname
						buf += ' '
						buf ++=
							(typ match {
								case StringType => "VARCHAR(255)"
								case IntegerType => "INT"
								case LongType => "BIGINT"
								case UUIDType => "UUID"
								case DateType => "DATE"
								case ReferenceType( _, _ ) => "BIGINT"
							})

						if (required)
							buf ++= " NOT NULL"

						if (unique)
							buf ++= " UNIQUE"
					}
				}

				columns.values foreach {
					case Column( fk, ReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= ", FOREIGN KEY ("
						buf ++= fk
						buf ++= ") REFERENCES "
						buf ++= ref
						buf ++= "(id)"
					case _ =>
				}
				
				buf ++= ");\n"
				
				columns.values foreach {
					case Column( c, _, _, _, _, true ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= c
						buf ++= ");\n"
					case Column( _, ArrayReferenceType(ref, _), _, _, _, _ ) =>
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

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, names, columns, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (id BIGSERIAL NOT NULL PRIMARY KEY"

				for (cname <- names) {
					val Column( _, typ, secret, required, unique, indexed ) = columns(cname)

					buf ++= ", "
					buf ++= cname
					buf += ' '
					buf ++=
						(typ match {
							case StringType => "VARCHAR(255)"
							case IntegerType => "INT"
							case LongType => "BIGINT"
							case UUIDType => "UUID"
							case DateType => "DATE"
							case ReferenceType( _, _ ) => "BIGINT"
						})

					if (required)
						buf ++= " NOT NULL"

					if (unique)
						buf ++= " UNIQUE"
				}

				columns.values foreach {
					case Column( fk, ReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= ", FOREIGN KEY ("
						buf ++= fk
						buf ++= ") REFERENCES "
						buf ++= ref
						buf ++= "(id)"
					case _ =>
				}

				buf ++= ");\n"

				columns.values foreach {
					case Column( c, _, _, _, _, true ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= c
						buf ++= ");\n"
					case Column( _, ArrayReferenceType(ref, _), _, _, _, _ ) =>
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

	def create( tables: List[Table] ) = {
		val buf = new StringBuilder

		tables foreach {
			case Table( name, names, columns, _, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= " (id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY"

				for (cname <- names) {
					val Column( _, typ, secret, required, unique, indexed ) = columns(cname)

					buf ++= ", "
					buf ++= cname
					buf += ' '
					buf ++=
						(typ match {
							case StringType => "VARCHAR(255)"
							case IntegerType => "INT"
							case LongType => "BIGINT"
							case UUIDType => "UUID"
							case DateType => "DATE"
							case ReferenceType( _, _ ) => "BIGINT"
						})

					if (required)
						buf ++= " NOT NULL"

					if (unique)
						buf ++= " UNIQUE"
				}

				columns.values foreach {
					case Column( fk, ReferenceType(ref, _), _, _, _, _ ) =>
						buf ++= ", FOREIGN KEY ("
						buf ++= fk
						buf ++= ") REFERENCES "
						buf ++= ref
						buf ++= "(id)"
					case _ =>
				}

				buf ++= ");\n"

				columns.values foreach {
					case Column( c, _, _, _, _, true ) =>
						buf ++= "CREATE INDEX ON "
						buf ++= name
						buf += '('
						buf ++= c
						buf ++= ");\n"
					case Column( _, ArrayReferenceType(ref, _), _, _, _, _ ) =>
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
		"H2 JDBC Driver" -> H2Database,
		"PostgreSQL Native Driver" -> PostgresDatabase,
		"MySQL Connector Java" -> MySQLDatabase
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

	def create( tables: List[Table] ): String
}
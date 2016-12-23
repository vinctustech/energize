package xyz.hyperreal.cras


object H2Database extends Database {
	def create( tables: List[Table] ) = {
		val buf = new StringBuilder
		
		tables foreach {
			case Table( name, names, columns, _ ) =>
				buf ++= "CREATE TABLE "
				buf ++= name
				buf ++= "(id IDENTITY NOT NULL PRIMARY KEY"
				
				for (cname <- names) {
					val Column( _, typ, secret, required, unique, indexed ) = columns(cname.toUpperCase)
					
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
							case TableType( _ ) => "BIGINT"
						})
						
					if (required)
						buf ++= " NOT NULL"
						
					if (unique)
						buf ++= " UNIQUE"
				}

				columns.values foreach {
					case Column( fk, TableType(ref), _, _, _, _ ) =>
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
					case _ =>
				}
		}
		
		buf.toString
	}
}

object Database {
	private val supported = Map[String, Database]( "H2" -> H2Database )
	
	def apply( name: String ) = supported get name
}

abstract class Database {
	def create( tables: List[Table] ): String
}
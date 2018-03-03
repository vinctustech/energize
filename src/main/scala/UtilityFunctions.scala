package xyz.hyperreal.energize2

import util.Random._
import xyz.hyperreal.table.TextTable
import xyz.hyperreal.importer.Importer
import xyz.hyperreal.bvm.VM


object UtilityFunctions {

	def rndPrintable( vm: VM, len: Int ) = new String( Array.fill( len )( util.Random.nextPrintableChar) )

	def rndAlpha( vm: VM, len: Int ) = new String( Array.fill( len )((util.Random.nextInt('z' - 'a') + 'a').toChar) )

	def rndInt( vm: VM, low: Int, high: Int ) = nextInt( high + 1 - low ) + low

	def show( vm: VM, tab: String ) = Console.print( TextTable(vm.statement.executeQuery(s"select * from $tab")) )

//	def Some( vm: VM, v: Any ) = scala.Some( v )

//	def jsrequest( vm: VM ): Unit = {
//		for (t <- env.tables.values)
//			env.jseng.put( t.name, new ResourceJSObject(env, t) )
//
//		env.jseng.put( "req", new RequestJSObject(env) )
//	}
//
//	def jscompile( vm: VM, code: String ) = {
//		jsrequest( env )
//		env.jseng.compile( code )
//	}
//
//	def jseval( vm: VM, code: String ) = {
//		jscompile( env, code ).eval
//	}

	def resourceSchema( vm: VM, resource: Resource ) = {
		val primitive: PartialFunction[FieldType, String] = {
			case CharType( len ) => s"char($len)"
			case StringType( len ) => s"string($len)"
			case TinytextType => "tinytext"
			case ShorttextType => "shorttext"
			case TextType => "text"
			case LongtextType => "longtext"
			case BooleanType => "boolean"
			case TinyintType => "tinyint"
			case SmallintType => "smallint"
			case IntegerType => "integer"
			case BigintType => "bigint"
			case FloatType => "float"
			case UUIDType => "UUID"
			case DateType => "date"
			case DatetimeType => "datetime"
			case TimeType => "time"
			case TimestampType => "timestamp"
			case BinaryType => "binary"
			case BLOBType( rep ) => s"blob($rep)"
			case DecimalType( precision, scale ) => s"decimal($precision,$scale)"
			case MediaType( Nil ) => "media"
			case MediaType( allowed ) => s"media(${allowed map {case MimeType(typ, subtype) => s"$typ/$subtype"} mkString ","})"
		}

		def field( t: FieldType ) =
			if (primitive isDefinedAt t)
				Map( "category" -> "primitive", "type" -> primitive(t) )
			else
				t match {
					case SingleReferenceType( _, res, _ ) => Map( "category" -> "one-to-many", "type" -> res )
					case ManyReferenceType( _, res, _ ) => Map( "category" -> "many-to-many", "type" -> res )
					case ArrayType( parm ) => Map( "category" -> "array", "type" -> primitive(parm) )
				}

		def modifiers( secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean ) =
			(if (secret) List("secret") else Nil) ++
			(if (required) List("required") else Nil) ++
			(if (unique) List("unique") else Nil) ++
			(if (indexed) List("indexed") else Nil)

		def fields( cs: List[Field] ) =
			cs map {case Field( fname, typ, secret, required, unique, indexed, _ ) =>	// todo: add validators support to schema
				Map( "name" -> fname, "type" -> field(typ), "modifiers" -> modifiers(secret, required, unique, indexed))}

		val Resource(rname, base, columns, _, visible, _, _, _, _) = resource

		Map( "name" -> rname, "resource" -> visible, "fields" -> fields(columns), "base" -> (base map path2string orNull) )
	}

	def databaseSchema( vm: VM ) = {
		val tables = vm.resources.values map {r => resourceSchema( vm, r )}

		Map( "tables" -> tables )
	}

	def populateDatabase( vm: VM, file: String ): Unit = {
		new Importer().importFromFile( file, false ) foreach { resource =>
			vm.resources.get( vm.db.desensitize(resource.name) ) match {
				case None =>
				case Some( t ) => t.batchInsert( resource.data, true )
			}
		}
	}
}

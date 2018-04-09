//@
package xyz.hyperreal.energize

import util.Random._
import xyz.hyperreal.table.TextTable
import xyz.hyperreal.importer.Importer
import xyz.hyperreal.bvm.VM


object UtilityFunctions {

//	def rndPrintable( vm: VM, len: Int ) = new String( Array.fill( len )( util.Random.nextPrintableChar) )

//	def rndAlpha( vm: VM, len: Int ) = new String( Array.fill( len )((util.Random.nextInt('z' - 'a') + 'a').toChar) )

//	def rndInt( vm: VM, low: Int, high: Int ) = nextInt( high + 1 - low ) + low

//	def show( vm: VM, tab: String ) = Console.print( TextTable(vm.statement.executeQuery(s"select * from $tab")) )

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
		def primitive( typ: FieldType ): Map[String, Any] = {
      val (name, args: Option[List[String]]) =
        typ match {
          case CharType( len ) => ("char", Some(List(len.toString)))
          case StringType( len ) => ("string", Some(List(len.toString)))
          case TinytextType => ("tinytext", None)
          case ShorttextType => ("shorttext", None)
          case TextType => ("text", None)
          case LongtextType => ("longtext", None)
          case BooleanType => ("boolean", None)
          case TinyintType => ("tinyint", None)
          case SmallintType => ("smallint", None)
          case IntegerType => ("integer", None)
          case BigintType => ("bigint", None)
          case FloatType => ("float", None)
          case UUIDType => ("UUID", None)
          case DateType => ("date", None)
          case DatetimeType => ("datetime", None)
          case TimeType => ("time", None)
          case TimestampType => ("timestamp", None)
          case BinaryType => ("binary", None)
          case BLOBType( rep ) => ("blob", Some(List(rep.name)))
          case DecimalType( precision, scale ) => ("decimal", Some(List(precision.toString, scale.toString)))
          case MediaType( Nil ) => ("media", Some(Nil))
          case MediaType( allowed ) => ("media", Some(allowed map {case MimeType(typ, subtype) => s"$typ/$subtype"}))
          case EnumType( name, elems ) => ("enum", Some(name :: elems.toList))
        }

      args match {
				case None => Map( "type" -> name )
				case Some( a ) => Map( "type" -> name, "parameters" -> a )
			}

    }

		def field( t: FieldType ) =
      t match {
        case SingleReferenceType( _, res, _ ) => Map( "category" -> "one-to-many", "type" -> res )
        case ManyReferenceType( _, res, _ ) => Map( "category" -> "many-to-many", "type" -> res )
        case ArrayType( parm ) => Map( "category" -> "array" ) ++ primitive( parm )
        case _ => Map( "category" -> "primitive" ) ++ primitive( t )
      }

		def modifiers( secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean ) =
			(if (secret) List("secret") else Nil) ++
			(if (required) List("required") else Nil) ++
			(if (unique) List("unique") else Nil) ++
			(if (indexed) List("indexed") else Nil)

		def fields( cs: List[Field] ) =
			cs map {case Field( fname, typ, secret, required, unique, indexed, _ ) =>	// todo: add validators support to schema
				Map( "name" -> fname, "type" -> field(typ), "modifiers" -> modifiers(secret, required, unique, indexed))}

		val Resource( rname, base, columns, _, visible, _, _ ) = resource

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

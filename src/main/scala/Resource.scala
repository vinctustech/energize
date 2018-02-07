package xyz.hyperreal.energize2

import java.sql.PreparedStatement

import scala.util.parsing.input.Position


object Resource {

}

//trait Method
//case object Get extends Method
//case object Post extends Method
//case object Put extends Method
//case object Patch extends Method
//case object Delete extends Method

case class Resource( name: String, fields: List[Field], fieldMap: Map[String, Field], visible: Boolean, manyToMany: Boolean, mediaArray: Boolean ) {
	var preparedInsert: PreparedStatement = _
	var preparedFullInsert: PreparedStatement = _

	def names = fields map (_.name)

	def init: Unit = {

	}
}

case class Field( name: String, typ: FieldType, secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean, validators: List[Int] )	// 'validators' is a list of function entry points

trait FieldType

trait ReferenceType extends FieldType {
	val resource: String
	var ref: Resource
}

trait PrimitiveFieldType extends FieldType
case object BooleanType extends PrimitiveFieldType
case class CharType( length: Int ) extends PrimitiveFieldType
case class StringType( length: Int ) extends PrimitiveFieldType
case object TinytextType extends PrimitiveFieldType
case object ShorttextType extends PrimitiveFieldType
case object TextType extends PrimitiveFieldType
case object LongtextType extends PrimitiveFieldType
case object TinyintType extends PrimitiveFieldType
case object SmallintType extends PrimitiveFieldType
case object IntegerType extends PrimitiveFieldType
case object BigintType extends PrimitiveFieldType
case object UUIDType extends PrimitiveFieldType
case object DateType extends PrimitiveFieldType
case object DatetimeType extends PrimitiveFieldType
case object TimeType extends PrimitiveFieldType
case object TimestampType extends PrimitiveFieldType
case object BinaryType extends PrimitiveFieldType
case object FloatType extends PrimitiveFieldType
case class BLOBType( rep: Symbol ) extends PrimitiveFieldType
case class DecimalType( precision: Int, scale: Int ) extends PrimitiveFieldType
case class MediaType( allowed: List[MimeType]/*, limit: Option[Int]*/ ) extends PrimitiveFieldType
case class ArrayType( parm: PrimitiveFieldType ) extends FieldType
case class SingleReferenceType( pos: Position, resource: String, var ref: Resource = null ) extends ReferenceType
case class ManyReferenceType( pos: Position, resource: String, var ref: Resource = null ) extends ReferenceType
case class EnumType( name: String, enum: Vector[String] ) extends PrimitiveFieldType//todo: implement enum type using 'data'

case class MimeType( typ: String, subtype: String )

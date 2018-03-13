package xyz.hyperreal.energize

import scala.util.parsing.input.Position


trait FieldType

trait ReferenceType extends FieldType {
  val resource: String
  var ref: Resource
}

trait PrimitiveFieldType extends FieldType
case object JSONType extends PrimitiveFieldType//todo: add json type
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

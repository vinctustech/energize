package xyz.hyperreal.energize2

import java.sql.{PreparedStatement, Statement}

import scala.util.parsing.input.Position


object Resource {

}

case class Resource( name: String, fields: List[Field], fieldMap: Map[String, Field], visible: Boolean,
										 manyToMany: Boolean, mediaArray: Boolean, statement: Statement, db: Database ) {
	var preparedInsert: PreparedStatement = _
	var preparedFullInsert: PreparedStatement = _
	var media: Resource = _

	def names = fields map (_.name)

//	def init: Unit = {
//
//	}

	//
	// aggregate functions
	//

	def count( filter: Option[String] ) = QueryFunctionHelpers.synchronized {
		val where = QueryFunctionHelpers.filtering( filter )
		val res = statement.executeQuery( s"SELECT COUNT(*) FROM $name $where" )

		res.next

		BigInt( res.getLong(1) )
	}

	def avg( filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( "AVG", this, filter, field )

	def sum( filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( "SUM", this, filter, field )

	def min( filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( "MIN", this, filter, field )

	def max( filter: Option[String], field: String ) = AggregateFunctionsHelpers.aggregate( "MAX", this, filter, field )

}

case class Field( name: String, typ: FieldType, secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean,
									validators: List[Int] )	//todo: 'validators' is a list of function entry points

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

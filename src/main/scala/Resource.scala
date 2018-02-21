package xyz.hyperreal.energize2

import java.sql.{Blob, Clob, PreparedStatement, Statement}

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
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

	//
	// query functions
	//

	def query( sql: Query, page: Option[String], start: Option[String], limit: Option[String],
						 allowsecret: Boolean ): List[OBJ] = {
		val res = QueryFunctionHelpers.synchronized( new Relation( db, statement.executeQuery(sql.query) ) )
		val list = new ListBuffer[OBJ]

		def mkOBJ( table: Resource, fields: List[String] ): OBJ = {
			val attr = new ListBuffer[(String, AnyRef)]
			val cols =
				if (fields == Nil)
					"_id" +: table.names
				else
					fields
			for (cn <- cols) {
				val obj = res.get( table.name, cn )

				//				if (dbtable == "")
				//					table.columnMap get dbcol match {
				//						case None =>
				//							attr += (dbcol -> obj)
				//						case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
				//							attr += (cname -> query( vm, reft,
				//								Query(s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref._id " +
				//									s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(table.name, "_id")}" +
				//									QueryFunctionHelpers.pageStartLimit(page, start, limit)), page, start, limit, allowsecret ))
				//						case Some( c ) => sys.error( s"data not from a table: matching column: ${c.name}" )
				//					}
				//				else

				//					vm.tables get dbtable match {
				//						case None if !(dbtable contains '$') => sys.error( s"data from an unknown table: $dbtable" )
				//					case None =>
				//						table.columns get dbcol match {
				//							case None => attr += (dbcol -> obj)
				//							case Some( Column(cname, ManyReferenceType(ref, reft), _, _, _, _) ) =>
				//								attr += (cname -> query( vm, reft,
				//									s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref._id " +
				//										s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(resource.db.desensitize("_id"))}" ))
				//							case Some( c ) => attr += (c.name -> obj)
				//						}
				//						case Some( t ) if t == table =>
				table.fieldMap get cn match {
					case None if cn == "_id" => attr += ("_id" -> obj.get)
					case None => sys.error( s"data from an unknown column: $cn" )
					case Some( Field(cname, SingleReferenceType(_, _, reft), _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> mkOBJ( reft, Nil ))
					case Some( Field(cname, ManyReferenceType(_, ref, reft), _, _, _, _, _) ) =>
						attr += (cname -> reft.query(
							Query(s"SELECT * FROM ${table.name}$$$ref INNER JOIN $ref ON ${table.name}$$$ref.$ref$$id = $ref.$idIn " +
								s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(table.name, "_id")}" +
								QueryFunctionHelpers.pageStartLimit(page, start, limit)), page, start, limit, allowsecret ))
					case Some( Field(cname, ArrayType(MediaType(_)), _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> obj.get.asInstanceOf[Array[AnyRef]].toList.map( m => s"/media/$m" ))
					case Some( Field(cname, ArrayType(_), _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> obj.get.asInstanceOf[Array[AnyRef]].toList)
					case Some( Field(cname, BinaryType, _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> bytes2hex( obj.get.asInstanceOf[Array[Byte]] ))
					case Some( Field(cname, BLOBType(rep), _, _, _, _, _) ) if obj.get ne null =>
						val blob = obj.get.asInstanceOf[Blob]
						val array = blob.getBytes( 0L, blob.length.toInt )

						rep match {
							case 'base64 => attr += (cname -> bytes2base64( array ))
							case 'hex => attr += (cname -> bytes2hex( array ))
							case 'list => attr += (cname -> array.toList)
						}
					case Some( Field(cname,TextType, _, _, _, _, _) ) if obj.get ne null =>
						val clob = obj.get.asInstanceOf[Clob]
						val s = clob.getSubString( 1L, clob.length.toInt )

						attr += (cname -> s)
					case Some( Field(cname, MediaType(_), _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> s"/media/${obj.get}")
					case Some( Field(cname, DatetimeType|TimestampType, _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> db.writeTimestamp( obj.get ))
					case Some( Field(cname, EnumType(_, enum), _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> enum(obj.get.asInstanceOf[Int]))
					case Some( Field(cname, DateType|TimeType|UUIDType, _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> obj.get.toString)
					case Some( Field(cname, _, true, _, _, _, _) ) =>
						if (allowsecret)
							attr += (cname -> obj.get)
					case Some( c ) =>
						attr += (c.name -> obj.get)
				}
			}

			ListMap( attr: _* )
		}

		while (res.next)
			list += mkOBJ( this, sql.fields )

		list.toList
	}

	def list( fields: Option[String], filter: Option[String], order: Option[String], page: Option[String],
						start: Option[String], limit: Option[String] ) = {
		val where = QueryFunctionHelpers.filtering( filter )
		val orderby =
			if (order.isEmpty)
				""
			else {
				" ORDER BY " +
					(QueryFunctionHelpers.DELIMITER.split( order.get ) map {
						o => {
							val QueryFunctionHelpers.ORDER(col, ordering) = o
							val ordering1 = ordering.toUpperCase

							s"${nameIn(col)} $ordering1"
						}
					} mkString ", ")
			}

		query( QueryFunctionHelpers.listQuery(this, fields, where + orderby, page, start, limit),
			Some("1"), None, None, false )
	}

	def findID( id: Long, fields: Option[String], page: Option[String], start: Option[String], limit: Option[String] ) =
		query( QueryFunctionHelpers.listQuery(this, fields, s" WHERE $name.$idIn = $id",
			page, start, limit), None, None, None, false )

	def findIDMany( id: Long, fields: String, page: Option[String], start: Option[String], limit: Option[String] ) =
		query( QueryFunctionHelpers.listQuery(this, Some(fields), s" WHERE $name.$idIn = $id",
			None, None, None), page, start, limit, false )

	def findValue( field: String, value: Any, allowsecret: Boolean ) =
		query( QueryFunctionHelpers.listQuery(this, None, s" WHERE $name.${nameIn(field)} = '$value'",
			None, None, None), None, None, None, allowsecret )

	def findOne( field: String, value: Any ) = findValue( field, value, false ).head

	def findOption( field: String, value: Any, allowsecret: Boolean ) =
		findValue( field, value, allowsecret ).headOption

	def findField( id: Long, field: String ) =
		query( Query(s"SELECT ${nameIn(field)} FROM $name WHERE $idIn = $id", List(field)), None, None, None, false ).head( field )

	def readField( id: Long, field: String ) = {
		val res = statement.executeQuery( s"SELECT ${nameIn(field)} FROM $name WHERE $idIn = $id" )

		res.next
		res.getObject( 1 )
	}

	//	def readBlob( resource: String, id: Long, field: String ) = {
	//		val res = resource.statement.executeQuery( s"SELECT ${nameIn(field)} FROM $resource WHERE $idIn = $id" )
	//
	//		res.next
	//
	//		val blob = res.getBlob( 1 )
	//
	//		blob.getBytes( 0, blob.length.toInt )
	//	}

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

//@
package xyz.hyperreal.energize

import java.sql.{Blob, Clob, Date => SQLDate, Time, PreparedStatement, Array => SQLArray}
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

import javax.sql.rowset.serial.{SerialBlob, SerialClob}

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer


object Resource {

}

case class Resource( name: String, base: Option[PathSegment], fields: List[Field], fieldMap: Map[String, Field], visible: Boolean,
										 manyToMany: Boolean, mediaArray: Boolean ) {
	var preparedInsert: PreparedStatement = _
	var preparedFullInsert: PreparedStatement = _
	var media: Resource = _
	var processor: Processor = _

	def names = fields map (_.name)

//	def init: Unit = {
//
//	}

	//
	// aggregate functions
	//

	def count( filter: Option[String] ) = QueryFunctionHelpers.synchronized {
		val where = QueryFunctionHelpers.filtering( filter )
		val res = processor.statement.executeQuery( s"SELECT COUNT(*) FROM $name $where" )

		res.next

		BigInt( res.getLong(1) )
	}

	def avg( filter: Option[String], field: String ) = AggregateFunctionHelpers.aggregate( "AVG", this, filter, field )

	def sum( filter: Option[String], field: String ) = AggregateFunctionHelpers.aggregate( "SUM", this, filter, field )

	def min( filter: Option[String], field: String ) = AggregateFunctionHelpers.aggregate( "MIN", this, filter, field )

	def max( filter: Option[String], field: String ) = AggregateFunctionHelpers.aggregate( "MAX", this, filter, field )

	//
	// query functions
	//

	def query( sql: Query, page: Option[String], start: Option[String], limit: Option[String],
						 allowsecret: Boolean ): List[OBJ] = {
		val res = processor.mutex.synchronized( new Relation( processor.db, processor.statement.executeQuery(sql.query) ) )
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
				//										s"WHERE ${table.name}$$$ref.${table.name}$$id = ${res.getLong(db.desensitize("_id"))}" ))
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
						val array =
							obj.get match {
								case o: SQLArray => o.getArray.asInstanceOf[Array[AnyRef]]
								case a: Array[AnyRef] => a
							}
						attr += (cname -> array.toList)
					case Some( Field(cname, BinaryType, _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> bytes2hex( obj.get.asInstanceOf[Array[Byte]] ))
					case Some( Field(cname, BLOBType(rep), _, _, _, _, _) ) if obj.get ne null =>
						val array =
							if (processor.db == PostgresDatabase)
								obj.get.asInstanceOf[Array[Byte]]
							else {
								val blob = obj.get.asInstanceOf[Blob]
								blob.getBytes(0L, blob.length.toInt)
							}

						rep match {
							case 'base64 => attr += (cname -> bytes2base64( array ))
							case 'hex => attr += (cname -> bytes2hex( array ))
							case 'list => attr += (cname -> array.toList)
						}
					case Some( Field(cname,TextType|TinytextType|ShorttextType|LongtextType, _, _, _, _, _) ) if obj.get != null && processor.db != PostgresDatabase =>
						val clob = obj.get.asInstanceOf[Clob]
						val s = clob.getSubString( 1L, clob.length.toInt )

						attr += (cname -> s)
					case Some( Field(cname, MediaType(_), _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> s"/media/${obj.get}")
					case Some( Field(cname, DatetimeType|TimestampType, _, _, _, _, _) ) if obj.get ne null =>
						attr += (cname -> processor.db.writeTimestamp( obj.get ))
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
		val res = processor.statement.executeQuery( s"SELECT ${nameIn(field)} FROM $name WHERE $idIn = $id" )

		res.next
		res.getObject( 1 )
	}

	//	def readBlob( resource: String, id: Long, field: String ) = {
	//		val res = statement.executeQuery( s"SELECT ${nameIn(field)} FROM $resource WHERE $idIn = $id" )
	//
	//		res.next
	//
	//		val blob = res.getBlob( 1 )
	//
	//		blob.getBytes( 0, blob.length.toInt )
	//	}

	//
	// command functions
	//


	def command( sql: String ) = processor.statement.executeUpdate( sql )

	def delete( id: Long ): Int = {
		if (mediaArray)
			fields foreach {
				case Field( n, ArrayType(MediaType(_)),_, _, _, _, _ ) =>
					for (mid <- readField( id, n ).asInstanceOf[Array[Long]])
						deleteMedia( mid )
				case _ =>
			}

		command( s"DELETE FROM $name WHERE $idIn = $id;" )
	}

	//todo: deal with resources that have media arrays as in delete()
	def deleteMany( filter: Option[String] ) = {
		command( s"DELETE FROM $name ${QueryFunctionHelpers.filtering( filter )}" )
	}

	def deleteMedia( id: Long ) = media.delete( id )

	def deleteValue( field: String, value: Any ) =
		value match {
			case s: String => command( s"DELETE FROM $name WHERE ${nameIn(field)} = '$s';" )
			case _ => command( s"DELETE FROM $name WHERE ${nameIn(field)} = $value;" )
		}

	def batchInsert( rows: List[Seq[AnyRef]], full: Boolean ) {
		val types = for ((c, i) <- fields zipWithIndex) yield (i + (if (full) 2 else 1), c.typ)
		val preparedStatement = if (full) preparedFullInsert else preparedInsert

		for (r <- rows) {
			if (full)
				preparedStatement.setInt( 1, r.head.asInstanceOf[java.lang.Integer] )

			for (((i, t), v) <- types zip (if (full) r.tail else r)) {
				(t, v) match {
					case (_, null) => CommandFunctionHelpers.setNull( preparedStatement, i, t )
					case (StringType( _ )|CharType( _ )|TextType|TinytextType|ShorttextType|LongtextType|UUIDType|TimeType|DateType|BinaryType, a: String) => preparedStatement.setString( i, a )
					case (IntegerType|TinyintType|SmallintType, a: Number) => preparedStatement.setInt( i, a.intValue )
					case (BigintType, a: Number) => preparedStatement.setLong( i, a.longValue )
					case (_: SingleReferenceType, a: java.lang.Integer) => preparedStatement.setInt( i, a )
					case (DecimalType( _, _ ), a: BigDecimal) => preparedStatement.setBigDecimal( i, a.underlying )
					case (DateType, d: LocalDate) => preparedStatement.setDate( i, SQLDate.valueOf(d) )
					case (TimeType, d: LocalTime) => preparedStatement.setTime( i, Time.valueOf(d) )
					case (DatetimeType|TimestampType, v: String) => preparedInsert.setTimestamp( i + 1, processor.db.readTimestamp(v.toString) )
					case (DatetimeType|TimestampType, d: LocalDateTime) => preparedInsert.setTimestamp( i + 1, processor.db.readTimestamp(v.toString) )
					case x => throw new BadRequestException( s"don't know what to do with $x, ${v.getClass}" )
				}
			}

			preparedStatement.addBatch
		}

		preparedStatement.executeBatch
		preparedStatement.clearParameters
	}

	def insert( json: OBJ ): Long = {
		val json1 = escapeQuotes( json )
		val cols = fields filterNot (_.typ.isInstanceOf[ManyReferenceType])
		val diff = json.keySet -- (cols map (_.name) toSet)

		if (diff nonEmpty)
			throw new BadRequestException( "insert: excess field(s): " + diff.mkString(", ") )

		// val mtms = fields filter (c => c.typ.isInstanceOf[ManyReferenceType])	// todo: mtm fields cannot be null; still needs to be checked

		for ((c, i) <- cols zipWithIndex) {
			json1 get c.name match {
				case None => CommandFunctionHelpers.setNull( preparedInsert, i + 1, c.typ )
				case Some( null ) => CommandFunctionHelpers.setNull( preparedInsert, i + 1, c.typ )
				case Some( v ) =>
					c.typ match {
						case SingleReferenceType( _, _, tref ) if !v.isInstanceOf[Int] && !v.isInstanceOf[Long] =>
							preparedInsert.setLong( i + 1, tref.findOne(CommandFunctionHelpers.uniqueField(tref), v).asInstanceOf[Map[String, Long]]("_id") )
						//								s"SELECT id FROM $tname WHERE " +
						//								(tref.columns.find(c => c.unique ) match {
						//									case None => throw new BadRequestException( "insert: no unique column in referenced resource in POST request" )
						//									case Some( uc ) => uc.name
						//								}) + " = '" + String.valueOf( v )

						//						case ManyReferenceType( _, _ ) =>
						//							if (v eq null)
						//								throw new BadRequestException( s"insert: manay-to-many field cannot be NULL: $c" )
						case SingleReferenceType( _, _, _ ) => preparedInsert.setLong( i + 1, v.asInstanceOf[Number].longValue )
						case BooleanType => preparedInsert.setBoolean( i + 1, v.asInstanceOf[Boolean] )
						case IntegerType|TinyintType|SmallintType => preparedInsert.setInt( i + 1, v.asInstanceOf[Number].intValue )
						case FloatType => preparedInsert.setDouble( i + 1, v.asInstanceOf[Number].doubleValue )
						case BigintType => preparedInsert.setLong( i + 1, v.asInstanceOf[Number].longValue )
						case DecimalType( _, _ ) =>
							v match {
								case v: java.lang.Double => preparedInsert.setBigDecimal( i + 1, new java.math.BigDecimal(v) )
								case b: BigDecimal => preparedInsert.setBigDecimal( i + 1, b.underlying )
							}
						case StringType( _ )|CharType( _ )|EnumType(_, _) => preparedInsert.setString( i + 1, v.toString )
						case DateType => preparedInsert.setDate( i + 1, SQLDate.valueOf(v.toString) )
						case TimeType => preparedInsert.setTime( i + 1, Time.valueOf(v.toString) )
						case UUIDType => preparedInsert.setObject( i + 1, UUID.fromString(v.toString) )
						case DatetimeType|TimestampType => preparedInsert.setTimestamp( i + 1, processor.db.readTimestamp(v.toString) )
						case ArrayType( MediaType(allowed) ) =>
							val s = v.asInstanceOf[Seq[String]] map (CommandFunctionHelpers.mediaInsert(this, allowed, _))

							preparedInsert.setObject( i + 1, s.asInstanceOf[Seq[java.lang.Long]].toArray )
						case ArrayType( prim ) =>
							preparedInsert.setArray( i + 1, processor.connection.createArrayOf(processor.db.array(prim), v.asInstanceOf[Seq[AnyRef]].toArray) )
						case BinaryType =>
							if (processor.db == PostgresDatabase) {
								println( (v.toString grouped 2 map (s => Integer.valueOf(s, 16) toByte) toArray) toList)
								preparedInsert.setBytes( i + 1, v.toString grouped 2 map (s => Integer.valueOf(s, 16) toByte) toArray )
							}
							else
								preparedInsert.setString( i + 1, v.toString )
						case BLOBType( rep ) =>
							val array =
								rep match {
									case 'base64 => base642bytes( v.toString )
									case 'hex => v.toString grouped 2 map (s => Integer.valueOf(s, 16) toByte) toArray
									case 'urlchars =>
										val data = v.toString

										if (data startsWith ";base64,")
											base642bytes( v.toString.substring(8) )
										else
											v.toString.getBytes
									case 'list => Array( v.asInstanceOf[Seq[Int]].map(a => a.toByte): _* )
								}

							if (processor.db == PostgresDatabase)
								preparedInsert.setBytes( i + 1, array )
							else
								preparedInsert.setBlob( i + 1, new SerialBlob(array) )
						case TextType|TinytextType|ShorttextType|LongtextType =>
							if (processor.db == PostgresDatabase)
								preparedInsert.setString( i + 1, v.toString )
							else
								preparedInsert.setClob( i + 1, new SerialClob(v.toString.toCharArray) )
						case MediaType( allowed ) =>
							preparedInsert.setLong( i + 1, CommandFunctionHelpers.mediaInsert(this, allowed, v.asInstanceOf[String]) )
					}
			}
		}

		val id =
			if (processor.db == PostgresDatabase) {
				val res = preparedInsert.executeQuery

				res.next
				res.getLong( 1 )
			} else {
				preparedInsert.executeUpdate
				//( Statement.RETURN_GENERATED_KEYS )
				val g = preparedInsert.getGeneratedKeys

				g.next
				g.getLong( 1 )
			}

		preparedInsert.clearParameters

		/* todo: this code allows values to be inserted into junction table for mtm fields
		val values = new ListBuffer[(String, String)]

		columns.foreach {
			case Column( col, ManyReferenceType(tab, ref), _, _, _, _ ) =>
				json get col match {
					case None =>
					case Some( vs ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							values += (s"(SELECT _id FROM $tab WHERE " +
								CommandFunctionHelpers.uniqueColumn( ref ) + " = '" + String.valueOf( v ) + "')" -> tab)
				}
			case _ =>
		}

		if (values nonEmpty) {
			values groupBy {case (_, r) => r} foreach {
				case (tab, vs) =>
					for ((v, _) <- vs)
						command( vm, s"INSERT INTO $name$$$tab VALUES ($id, $v)" )
			}
		}
*/

		id
	}

	def update( id: Long, json: OBJ, all: Boolean ) = {
		val fields1 = fields filterNot (c => c.typ.isInstanceOf[ManyReferenceType]) map (c => c.name) toSet
		val mediaDeletes = new ListBuffer[Long]

		if (all && json.keySet != fields1)
			if ((fields1 -- json.keySet) nonEmpty)
				throw new BadRequestException( "update: missing field(s): " + (fields1 -- json.keySet).mkString( ", " ) )
			else
				throw new BadRequestException( "update: excess field(s): " + (json.keySet -- fields1).mkString( ", " ) )
		else {
			val com = new StringBuilder( "UPDATE " )
			//			var typ: ColumnType = null

			com ++= name
			com ++= " SET "
			com ++=
				(for ((k, v) <- escapeQuotes( json ).toList)
					yield {
						val kIn = nameIn( k )

						fieldMap(k).typ match {
							case DatetimeType|TimestampType => kIn + " = '" + processor.db.readTimestamp( v.toString ) + "'"
							case UUIDType|TimeType|DateType|StringType( _ )|CharType( _ )|BinaryType|EnumType(_, _) if v ne null => s"$kIn = '$v'"//todo: escape string (taylored for database)
							case TextType|TinytextType|ShorttextType|LongtextType => throw new BadRequestException( "updating a text field isn't supported yet" )
							case ArrayType( _ ) =>
								kIn + " = " + v.asInstanceOf[Seq[Any]].
									map {
										case e: String => s"'$e'"//todo: escape string (tailored for database)
										case e => String.valueOf( e )
									}.mkString( processor.db.arrayOpen, ", ", processor.db.arrayClose )
							case BLOBType( _ ) => throw new BadRequestException( "updating a blob field isn't supported yet" )
							case MediaType( allowed ) =>
								val oldid = readField( id, k ).asInstanceOf[Long]
								val newid = CommandFunctionHelpers.mediaInsert( this, allowed, v.asInstanceOf[String] )

								mediaDeletes += oldid
								kIn + " = " + String.valueOf( newid )
							case t: SingleReferenceType if v ne null =>
								if (v.isInstanceOf[Int] || v.isInstanceOf[Long])
									s"$kIn = $v"
								else {
									val reft = t.asInstanceOf[SingleReferenceType].ref
									val refc = nameIn( CommandFunctionHelpers.uniqueField(reft) )

									s"$kIn = (SELECT $idIn FROM ${reft.name} WHERE $refc = '$v')"
								}
							case _ => kIn + " = " + String.valueOf( v )
						}
					}) mkString ", "
			com ++= s" WHERE $idIn = "
			com ++= id.toString

			val res = processor.statement.executeUpdate( com.toString )

			for (id <- mediaDeletes)
				deleteMedia( id )

			res
		}
	}

	def arrayInsert( id: Long, field: String, idx: Int, json: OBJ ): Unit = {
		json get "data" match {
			case None => throw new BadRequestException( "arrayInsert: 'data' not found in JSON body" )
			case Some( data ) =>
				val oldarray = readField( id, field ).asInstanceOf[Array[Any]]

				if (idx < 0 || idx > oldarray.length)
					throw new BadRequestException( s"arrayInsert: array index is out of range: $idx" )

				val newarray = ListBuffer[Any]( oldarray.view(0, idx): _* )
				val item =
					fieldMap get field match {
						case None => throw new NotFoundException( s"arrayInsert: '$field' not found" )
						case Some( Field(_, ArrayType(MediaType(allowed)), _, _, _, _, _) ) =>
							CommandFunctionHelpers.mediaInsert( this, allowed, data.toString )
						case _ => data
					}

				newarray += item
				newarray ++= oldarray.view( idx, oldarray.length )
				update( id, Map(field -> newarray), false )
		}
	}

	def arrayUpdate( id: Long, field: String, idx: Int, json: OBJ ): Unit = {
		json get "data" match {
			case None => throw new BadRequestException( "arrayUpdate: 'data' not found in JSON body" )
			case Some( data ) =>
				val oldarray = readField( id, field ).asInstanceOf[Array[Any]]

				if (idx < 0 || idx > oldarray.length)
					throw new BadRequestException( s"arrayUpdate: array index is out of range: $idx" )

				val newarray = ListBuffer[Any]( oldarray: _* )
				val (item, todelete) =
					fieldMap get field match {
						case None => throw new NotFoundException( s"arrayUpdate: '$field' not found" )
						case Some( Field(_, ArrayType(MediaType(allowed)), _, _, _, _, _) ) =>
							(CommandFunctionHelpers.mediaInsert( this, allowed, data.toString ), Some( oldarray(idx).asInstanceOf[Long] ))
						case _ => (data, None)
					}

				newarray(idx) = item
				update( id, Map(field -> newarray), false )

				if (todelete isDefined)
					media.delete( todelete get )
		}
	}

	def arrayDelete( id: Long, field: String, idx: Int ): Unit = {
		val oldarray = readField( id, field ).asInstanceOf[Array[Any]]

		if (idx < 0 || idx > oldarray.length)
			throw new BadRequestException( s"arrayDelete: array index is out of range: $idx" )

		val newarray = ListBuffer[Any]( oldarray: _* )
		val todelete =
			fieldMap get field match {
				case None => throw new NotFoundException( s"arrayDelete: '$field' not found" )
				case Some( Field(_, ArrayType(MediaType(_)), _, _, _, _, _) ) =>
					Some( oldarray(idx).asInstanceOf[Long] )
				case _ => None
			}

		newarray remove idx
		update( id, Map(field -> newarray), false )

		if (todelete isDefined)
			media.delete( todelete get )
	}

	def insertLinks( id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new BadRequestException( s"insertLinks: field not found: $field" )
			case Some( vs ) =>
				fieldMap(field).typ match {
					case ManyReferenceType( _, _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							associateID( id, ref, CommandFunctionHelpers.uniqueField(ref), v )
					case _ => throw new BadRequestException( s"insertLinks: field not many-to-many: $field" )
				}
		}

	def append( id: Long, field: String, json: OBJ ) = {
		fieldMap get field match {
			case Some( Field(_, ManyReferenceType(_, _, ref), _, _, _, _, _) ) =>
				val tid = ref.insert( json )

				associateIDs( id, ref, tid )
				tid
			case Some( _ ) => throw new BadRequestException( s"append: field not many-to-many: $field" )
			case None => throw new BadRequestException( s"append: field not found: $field" )
		}
	}

	def appendIDs( sid: Long, field: String, tid: Long ) =
		fieldMap get field match {
			case Some( Field(_, ManyReferenceType(_, _, ref), _, _, _, _, _) ) =>
				associateIDs( sid, ref, tid )
			case Some( _ ) => throw new BadRequestException( s"appendIDs: field not many-to-many: $field" )
			case None => throw new BadRequestException( s"appendIDs: field not found: $field" )
		}

	def deleteLinks( id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new BadRequestException( s"append: field not found: $field" )
			case Some( vs ) =>
				fieldMap(field).typ match {
					case ManyReferenceType( _, _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							deleteLinkID( id, ref, CommandFunctionHelpers.uniqueField( ref ), v )
					case _ => throw new BadRequestException( s"append: field not many-to-many: $field" )
				}
		}

	def deleteLinksID( id: Long, field: String, tid: Long ) =
		fieldMap(field).typ match {
			case ManyReferenceType( _, _, ref ) => deleteLinkIDs( id, ref, tid )
			case _ => throw new BadRequestException( s"append: field not many-to-many: $field" )
		}

	def deleteLinkID( id: Long, dst: Resource, dfield: String, dvalue: AnyRef ) = {
		val did = dst.findOne( dfield, dvalue )( "_id" ).asInstanceOf[Long]

		deleteLinkIDs( id, dst, did )
	}

	def deleteLinkIDs( sid: Long, dst: Resource, did: Long ) =
		command( s"DELETE FROM $name$$${dst.name} WHERE $name$$id = $sid AND ${dst.name}$$id = $did" )

	def associateID( id: Long, dst: Resource, dfield: String, dvalue: AnyRef ) = {
		val did = dst.findOne( dfield, dvalue )( "_id" ).asInstanceOf[Long]

		associateIDs( id, dst, did )
	}

	def associateIDs( sid: Long, dst: Resource, did: Long ) =
		command( s"INSERT INTO $name$$${dst.name} VALUES ($sid, $did)" )

	def associate( sfield: String, svalue: AnyRef, dst: Resource, dfield: String, dvalue: AnyRef ) = {
		val sobj = findOne( sfield, svalue )( "_id" ).asInstanceOf[Long]

		associateID( sobj, dst, dfield, dvalue )
	}

}

case class Field( name: String, typ: FieldType, secret: Boolean, required: Boolean, unique: Boolean, indexed: Boolean,
									validators: List[Int] )	//todo: 'validators' is a list of function entry points

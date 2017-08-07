package xyz.hyperreal.energize

import java.sql.{Date, PreparedStatement, Types}
import java.time.LocalDate
import javax.sql.rowset.serial.{SerialBlob, SerialClob}

import scala.collection.mutable.ListBuffer


object CommandFunctionHelpers {

	val DATAURL = """data:(?:([-a-z]+)/([-.a-z0-9]+)(;[a-zA-Z]+=[-a-zA-Z0-9]+)?)?(;base64)?"""r

	def dataurl( s: String ): Either[String, (String, String, String, String)] = {
		def param( s: String ) = if (s eq null) "" else s

		s.indexOf( ',' ) match {
			case -1 => Left( "invalid data URL: no comma" )
			case idx =>
				val data = s.substring( idx + 1 )

				s.substring( 0, idx ) match {
					case DATAURL( null, _, p, null ) => Right( ("text", "plain", param( p ), data) )
					case DATAURL( typ, subtype, p, null ) => Right( (typ, subtype, param( p ), data) )
					case DATAURL( null, _, p, _ ) => Right( ("text", "plain", param( p ), ";base64," + data) )
					case DATAURL( typ, subtype, p, _ ) => Right( (typ, subtype, param( p ), ";base64," + data) )
					case header => Left( s"invalid data URL: '$header'" )
				}
		}
	}

	// RFC2396 section 2.3
	//	unreserved  = alphanum | mark
	//
	//	mark        = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"

	// RFC2396 section 3.0
	// uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","

	def decode( urlchars: String ): Either[String, String] = {
		val res = new StringBuilder

		def hexchar( c: Char ) = "0123456789abcdefABCDEF" contains c

		def _decode( s: Stream[Char] ): Option[String] =
			s match {
				case Stream.Empty => None
				case h #:: t =>
					if ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.!~*'()" contains h) {
						res += h
						_decode( t )
					} else if (h == '%')
						if (t != Stream.empty && hexchar( t.head ) && t.tail != Stream.empty && hexchar( t.tail.head )) {
							res += Integer.parseUnsignedInt( t take 2 mkString, 16).toChar
							_decode (t.tail.tail)
						} else
							Some( s"invalid data URL escape sequence: '${(h #:: t) take 3 mkString}'" )
					else if (h == '+') {
						res += ' '
						_decode( t )
					} else
						Some( s"invalid character in data URL: '$h'" )
			}

		_decode( urlchars toStream ) match {
			case None => Right( res toString )
			case Some( error ) => Left( error )
		}
	}

	def uniqueColumn( resource: Table ) =
		resource.columns find (_.unique ) match {
			case None => throw new BadRequestException( s"insert: no unique column in '${resource.name}'" )
			case Some( uc ) => uc.name
		}

	def mediaInsert( env: Environment, allowed: List[MimeType], v: String ) =
		dataurl( v ) match {
			case Left( error ) => throw new BadRequestException( error )
			case Right( (typ, subtype, parameter, data) ) =>
				if (allowed != Nil && !allowed.exists {case MimeType(atyp, asubtype) => typ == atyp && (asubtype == "*" || subtype == asubtype)})
					throw new BadRequestException( s"insert: MIME type not allowed: $typ/$subtype" )

				CommandFunctions.insert( env, env table "_media_", Map("type" -> s"$typ/$subtype$parameter", "data" -> data) )
		}

	def setNull( preparedStatement: PreparedStatement, col: Int, typ: ColumnType ): Unit = {
		val t =
			typ match {
				case BooleanType => Types.BOOLEAN
				case StringType => Types.VARCHAR
				case TextType => Types.CLOB
				case IntegerType => Types.INTEGER
				case FloatType => Types.FLOAT
				case LongType|SingleReferenceType( _, _ )|MediaType( _, _, _ ) => Types.BIGINT
				case BLOBType( _ ) => Types.BLOB
				case TimestampType => Types.TIMESTAMP
				case ArrayType( _, _, _, _ ) => Types.ARRAY
				case DecimalType( _, _ ) => Types.DECIMAL
				case DateType => Types.DATE
			}

		preparedStatement.setNull( col, t )
	}

}

object CommandFunctions {

	def command( env: Environment, sql: String ) = env.statement.executeUpdate( sql )
	
	def delete( env: Environment, resource: Table, id: Long ) = command( env, s"DELETE FROM ${resource.name} WHERE $idIn = $id;" )

	def deleteMany( env: Environment, resource: Table, filter: Option[String] ) = command( env, s"DELETE FROM ${resource.name} ${QueryFunctionHelpers.filtering(filter)}" )

	def deleteValue( env: Environment, resource: Table, field: String, value: Any ) =
		value match {
			case s: String => command( env, s"DELETE FROM ${resource.name} WHERE ${nameIn(field)} = '$s';" )
			case _ => command( env, s"DELETE FROM ${resource.name} WHERE ${nameIn(field)} = $value;" )
		}

	def batchInsert( env: Environment, resource: Table, rows: List[Seq[AnyRef]], full: Boolean ) {
		val types = for ((c, i) <- resource.columns zipWithIndex) yield (i + (if (full) 2 else 1), c.typ)
		val preparedStatement = if (full) resource.preparedFullInsert else resource.preparedInsert

		for (r <- rows) {
			if (full)
				preparedStatement.setInt( 1, r.head.asInstanceOf[java.lang.Integer] )

			for (((i, t), v) <- types zip (if (full) r.tail else r)) {
				(t, v) match {
					case (_, null) => CommandFunctionHelpers.setNull( preparedStatement, i, t )
					case (StringType|TextType, a: String) => preparedStatement.setString( i, a )
					case (IntegerType, a: java.lang.Integer) => preparedStatement.setInt( i, a )
					case (_: SingleReferenceType, a: java.lang.Integer) => preparedStatement.setInt( i, a )
					case (LongType, a: java.lang.Long) => preparedStatement.setLong( i, a )
					case (DecimalType( _, _ ), a: BigDecimal) => preparedStatement.setBigDecimal( i, a.underlying )
					case (DateType, a: LocalDate) => preparedStatement.setDate( i, Date.valueOf(a) )
					case x => throw new BadRequestException( s"don't know what to do with $x, ${v.getClass}" )
				}
			}

			preparedStatement.addBatch
		}
		
		preparedStatement.executeBatch
		preparedStatement.clearParameters
	}
	
	def insert( env: Environment, resource: Table, json: OBJ ): Long = {
		val json1 = escapeQuotes( json )
		val cols = resource.columns filterNot (c => c.typ.isInstanceOf[ManyReferenceType])
		val diff = json.keySet -- (cols map (_.name) toSet)

		if (diff nonEmpty)
			throw new BadRequestException( "insert: excess field(s): " + diff.mkString(", ") )

		val mtms = resource.columns filter (c => c.typ.isInstanceOf[ManyReferenceType])	// todo: mtm fields cannot be null; still needs to be checked

		for ((c, i) <- cols zipWithIndex) {
			json1 get c.name match {
				case None => CommandFunctionHelpers.setNull( resource.preparedInsert, i + 1, c.typ )
				case Some( null ) => CommandFunctionHelpers.setNull( resource.preparedInsert, i + 1, c.typ )
				case Some( v ) =>
					c.typ match {
						case SingleReferenceType( _, tref ) if !v.isInstanceOf[Int] && !v.isInstanceOf[Long] =>
							resource.preparedInsert.setLong( i + 1, QueryFunctions.findOne(env, tref, CommandFunctionHelpers.uniqueColumn(tref), v).asInstanceOf[Map[String, Long]]("_id") )
//								s"SELECT id FROM $tname WHERE " +
//								(tref.columns.find(c => c.unique ) match {
//									case None => throw new BadRequestException( "insert: no unique column in referenced resource in POST request" )
//									case Some( uc ) => uc.name
//								}) + " = '" + String.valueOf( v )

//						case ManyReferenceType( _, _ ) =>
//							if (v eq null)
//								throw new BadRequestException( s"insert: manay-to-many field cannot be NULL: $c" )
						case SingleReferenceType( _, _ ) => resource.preparedInsert.setLong( i + 1, v.asInstanceOf[Number].longValue )
						case BooleanType => resource.preparedInsert.setBoolean( i + 1, v.asInstanceOf[Boolean] )
						case IntegerType => resource.preparedInsert.setInt( i + 1, v.asInstanceOf[Number].intValue )
						case FloatType => resource.preparedInsert.setDouble( i + 1, v.asInstanceOf[Number].doubleValue )
						case LongType => resource.preparedInsert.setLong( i + 1, v.asInstanceOf[Number].longValue )
						case UUIDType | TimeType | DateType | BinaryType | StringType | EnumType(_, _) => resource.preparedInsert.setString( i + 1, v.toString )
						case DatetimeType | TimestampType => resource.preparedInsert.setTimestamp( i + 1, env.db.readTimestamp(v.toString) )
						case ArrayType( MediaType(allowed, _, _), _, _, _ ) =>
							val s = v.asInstanceOf[Seq[String]] map (CommandFunctionHelpers.mediaInsert(env, allowed, _))

							resource.preparedInsert.setObject( i + 1, s.asInstanceOf[Seq[java.lang.Long]].toArray )
						case ArrayType( _, _, _, _ ) => resource.preparedInsert.setObject( i + 1, v.asInstanceOf[Seq[Any]].toArray )
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

							resource.preparedInsert.setBlob( i + 1, new SerialBlob(array) )
						case TextType => resource.preparedInsert.setClob( i + 1, new SerialClob(v.toString.toCharArray) )
						case MediaType( allowed, _, limit ) =>
							resource.preparedInsert.setLong( i + 1, CommandFunctionHelpers.mediaInsert(env, allowed, v.asInstanceOf[String]) )
					}
			}
		}

		val id =
//			if (env.db == PostgresDatabase) {
//				val res = env.statement.executeQuery( com + "RETURNING _id" )
//
//				res.next
//				res.getLong( 1 )
//			} else {
			{
				resource.preparedInsert.executeUpdate
//( Statement.RETURN_GENERATED_KEYS )
				val g = resource.preparedInsert.getGeneratedKeys

				g.next

				val res = g.getLong( 1 )

				resource.preparedInsert.clearParameters
				res
			}

		/* todo: this code allows values to be inserted into junction table for mtm columns
		val values = new ListBuffer[(String, String)]

		resource.columns.foreach {
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
						command( env, s"INSERT INTO ${resource.name}$$$tab VALUES ($id, $v)" )
			}
		}
*/

		id
	}
	
	def update( env: Environment, resource: Table, id: Long, json: OBJ, all: Boolean ) = {
		val fields = resource.columns filterNot (c => c.typ.isInstanceOf[ManyReferenceType]) map (c => c.name) toSet
		val mediaDeletes = new ListBuffer[Long]

		if (all && json.keySet != fields)
			if ((fields -- json.keySet) nonEmpty)
				throw new BadRequestException( "update: missing field(s): " + (fields -- json.keySet).mkString( ", " ) )
			else
				throw new BadRequestException( "update: excess field(s): " + (json.keySet -- fields).mkString( ", " ) )
		else {
			val com = new StringBuilder( "UPDATE " )
			//			var typ: ColumnType = null

			com ++= resource.name
			com ++= " SET "
			com ++=
				(for ((k, v) <- escapeQuotes( json ).toList)
					yield {
						val kIn = nameIn( k )

						resource.columnMap(k).typ match {
							case DatetimeType | TimestampType => kIn + " = '" + env.db.readTimestamp( v.toString ) + "'"
							case UUIDType | TimeType | DateType | StringType | BinaryType | EnumType(_, _) if v ne null => s"$kIn = '$v'"
							case TextType => throw new BadRequestException( "updating a text field isn't supported yet" )
							case ArrayType( _, _, _, _ ) => kIn + " = " + v.asInstanceOf[Seq[Any]].mkString( "(", ", ", ")" )
							case BLOBType( _ ) => throw new BadRequestException( "updating a blob field isn't supported yet" )
							case MediaType( allowed, _, _ ) =>
								val oldid = QueryFunctions.readID( env, resource, id, k )
								val newid = CommandFunctionHelpers.mediaInsert( env, allowed, v.asInstanceOf[String] )

								mediaDeletes += oldid
								kIn + " = " + String.valueOf( newid )
							case t: SingleReferenceType if v ne null =>
								if (v.isInstanceOf[Int] || v.isInstanceOf[Long])
									s"$kIn = $v"
								else {
									val reft = t.asInstanceOf[SingleReferenceType].ref
									val refc = nameIn( CommandFunctionHelpers.uniqueColumn(reft) )

									s"$kIn = (SELECT $idIn FROM ${reft.name} WHERE $refc = '$v')"
								}
							case _ => kIn + " = " + String.valueOf( v )
						}
					}) mkString ", "
			com ++= s" WHERE $idIn = "
			com ++= id.toString

			val res = env.statement.executeUpdate( com.toString )

			for (id <- mediaDeletes)
				delete( env, env table "_media_", id )//todo: put the Table object for _media_ somewhere so it doesn't have to be looked up

			res
		}
	}

	def arrayInsert( env: Environment, resource: Table, id: Long, field: String, idx: Int, json: OBJ ): Unit = {
		json get "data" match {
			case None => throw new BadRequestException( "arrayInsert: 'data' field not found" )
			case Some( data ) =>
		}
	}

	def insertLinks( env: Environment, resource: Table, id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new BadRequestException( s"insertLinks: field not found: $field" )
			case Some( vs ) =>
				resource.columnMap(field).typ match {
					case ManyReferenceType( _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							associateID( env, resource, id, ref, CommandFunctionHelpers.uniqueColumn(ref), v )
					case _ => throw new BadRequestException( s"insertLinks: field not many-to-many: $field" )
				}
		}

	def append( env: Environment, resource: Table, id: Long, field: String, json: OBJ ) = {
		resource.columnMap get field match {
			case Some( Column(_, ManyReferenceType(_, ref), _, _, _, _, _) ) =>
				val tid = insert( env, ref, json )

				associateIDs( env, resource, id, ref, tid )
				tid
			case Some( _ ) => throw new BadRequestException( s"append: field not many-to-many: $field" )
			case None => throw new BadRequestException( s"append: field not found: $field" )
		}
	}

	def appendIDs( env: Environment, src: Table, sid: Long, field: String, tid: Long ) =
		src.columnMap get field match {
			case Some( Column(_, ManyReferenceType(_, ref), _, _, _, _, _) ) =>
				associateIDs( env, src, sid, ref, tid )
			case Some( _ ) => throw new BadRequestException( s"appendIDs: field not many-to-many: $field" )
			case None => throw new BadRequestException( s"appendIDs: field not found: $field" )
		}

	def deleteLinks( env: Environment, resource: Table, id: Long, field: String, json: OBJ ) =
		json get field match {
			case None => throw new BadRequestException( s"append: field not found: $field" )
			case Some( vs ) =>
				resource.columnMap(field).typ match {
					case ManyReferenceType( _, ref ) =>
						for (v <- vs.asInstanceOf[List[AnyRef]])
							deleteLinkID( env, resource, id, ref, CommandFunctionHelpers.uniqueColumn( ref ), v )
					case _ => throw new BadRequestException( s"append: field not many-to-many: $field" )
				}
		}

	def deleteLinksID( env: Environment, resource: Table, id: Long, field: String, tid: Long ) =
		resource.columnMap(field).typ match {
			case ManyReferenceType( _, ref ) => deleteLinkIDs( env, resource, id, ref, tid )
			case _ => throw new BadRequestException( s"append: field not many-to-many: $field" )
		}

	def deleteLinkID( env: Environment, src: Table, id: Long, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val did = QueryFunctions.findOne( env, dst, dfield, dvalue )( "_id" ).asInstanceOf[Long]

		deleteLinkIDs( env, src, id, dst, did )
	}

	def deleteLinkIDs( env: Environment, src: Table, sid: Long, dst: Table, did: Long ) =
		command( env, s"DELETE FROM ${src.name}$$${dst.name} WHERE ${src.name}$$id = $sid AND ${dst.name}$$id = $did" )

	def associateID( env: Environment, src: Table, id: Long, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val did = QueryFunctions.findOne( env, dst, dfield, dvalue )( "_id" ).asInstanceOf[Long]

		associateIDs( env, src, id, dst, did )
	}

	def associateIDs( env: Environment, src: Table, sid: Long, dst: Table, did: Long ) =
		command( env, s"INSERT INTO ${src.name}$$${dst.name} VALUES ($sid, $did)" )

	def associate( env: Environment, src: Table, sfield: String, svalue: AnyRef, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val sobj = QueryFunctions.findOne( env, src, sfield, svalue )( "_id" ).asInstanceOf[Long]

		associateID( env, src, sobj, dst, dfield, dvalue )
	}
}
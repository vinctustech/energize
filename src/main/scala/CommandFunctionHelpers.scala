package xyz.hyperreal.energize

import java.sql.{PreparedStatement, Types}


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

	def uniqueField( resource: Resource ) =
		resource.fields find (_.unique ) match {
			case None => throw new BadRequestException( s"insert: no unique column in '${resource.name}'" )
			case Some( uc ) => uc.name
		}

	def mediaInsert( resource: Resource, allowed: List[MimeType], v: String ) =
		dataurl( v ) match {
			case Left( error ) => throw new BadRequestException( error )
			case Right( (typ, subtype, parameter, data) ) =>
				if (allowed != Nil && !allowed.exists {case MimeType(atyp, asubtype) => typ == atyp && (asubtype == "*" || subtype == asubtype)})
					throw new BadRequestException( s"insert: MIME type not allowed: $typ/$subtype" )

				resource.media.insert( Map("type" -> s"$typ/$subtype$parameter", "data" -> data) )
		}

	def setNull( preparedStatement: PreparedStatement, col: Int, typ: FieldType ): Unit = {
		preparedStatement.setNull( col, field2types(typ) )
	}

}

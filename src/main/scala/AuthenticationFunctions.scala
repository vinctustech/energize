//@
package xyz.hyperreal.energize

import org.mindrot.jbcrypt.BCrypt
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}
import xyz.hyperreal.bvm.VM
import xyz.hyperreal.json.{DefaultJSONReader}

import scala.util.{Failure, Success}
import scala.util.parsing.input.Position


object AuthenticationFunctionHelpers {

	val CREDENTIALS = "(.*):(.*)"r
	lazy val SCHEME = AUTHENTICATION.getString( "scheme" )
	lazy val KEY = AUTHENTICATION.getString( "key" )
	lazy val EXPIRATION = if (AUTHENTICATION.getIsNull( "expiration" )) Int.MaxValue else AUTHENTICATION.getInt( "expiration" )

	SCHEME match {
		case "JWT" =>
		case _ => sys.error( """authentication scheme can only be "JWT"""" )
	}

	if (EXPIRATION <= 0)
		sys.error( "authentication expiration value must be positive" )

	def encodeToken( userid: Long ) = Jwt.encode( JwtClaim(s"""{"user":$userid}""").issuedNow.expiresIn(EXPIRATION), KEY, JwtAlgorithm.HS256 )

}

object AuthenticationFunctions {

	def authorization( vm: VM, req: OBJ ) = {
		val token =
			req("get").asInstanceOf[(VM, Position, List[Position], Any) => String]( null, null, null, "Authorization" ) match {
				case null => throw new UnauthorizedException( "no web token in request" )
				case h =>
					if (!h.startsWith("Bearer "))
						throw new UnauthorizedException( """expected "Bearer" token in request""" )
					else
						h.substring( 7 ).trim
			}

		Jwt.decode( token, AuthenticationFunctionHelpers.KEY, Seq(JwtAlgorithm.HS256) ) match {
			case Success( payload ) =>
				val user = DefaultJSONReader.fromString( payload )("user").asInstanceOf[Number].longValue

				vm.users.findID( user, None, None, None, None ) match {
					case Nil => throw new NotFoundException( s"user id $user not found" )
					case List( u ) => u
				}
			case Failure( exception ) => throw new UnauthorizedException( exception.getMessage )
		}
	}

	def register( vm: VM, json: OBJ ) = {
		val required = vm.users.names.toSet -- Set( "createdTime", "updatedTime", "state", "groups" )

		if ((required -- json.keySet) nonEmpty)
			throw new BadRequestException( "register: missing field(s): " + (required -- json.keySet).mkString(", ") )

		(json("email"), json("password")) match {
			case (null|"", _) => throw new BadRequestException( "email may not be null or empty" )
			case (_, null|"") => throw new BadRequestException( "password may not be null or empty" )
			case _ =>
		}

		val json1: OBJ =
			(json map {
				case ("password", p: String) => ("password", BCrypt.hashpw( p, BCrypt.gensalt ))
				case f => f
			}) + ("groups" -> List("user")) + ("createdTime" -> now)

		AuthenticationFunctionHelpers.encodeToken( vm.users.insert(json1) )
	}

	def login( vm: VM, json: OBJ ) = {
		val required = Set( "email", "password" )

		if ((required -- json.keySet) nonEmpty)
			throw new BadRequestException( "login: missing field(s): " + (required -- json.keySet).mkString(", ") )

		if ((json.keySet -- required) nonEmpty)
			throw new BadRequestException( "register: excess field(s): " + (required -- json.keySet).mkString(", ") )

		val email = json("email")

		def denied = throw new UnauthorizedException( "email or password doesn't match" )

		vm.users.findOption( "email", email, true ) match {
			case None => denied
			case Some( u ) =>
				if (BCrypt.checkpw( json("password").asInstanceOf[String], u("password").asInstanceOf[String] ))
					AuthenticationFunctionHelpers.encodeToken( u("_id").asInstanceOf[Long] )
				else
					denied
		}
	}

	def authorize( vm: VM, group: Option[String], req: OBJ ) {
		val key = req("query").asInstanceOf[Map[String, String]] get "access_token"

		if (key.isEmpty) {
			val user = authorization( vm, req )

			if (group.nonEmpty && !user( "groups" ).asInstanceOf[List[String]].contains( group.get ))
				throw new UnauthorizedException( s"""not a member of group "${group.get}"""" )
		} else
			access( vm, key )
	}

	def access( vm: VM, key: Option[String] ): Unit =
		if (key.isEmpty || key.get != vm.key)
			throw new ForbiddenException( "wrong or missing access_token" )

}

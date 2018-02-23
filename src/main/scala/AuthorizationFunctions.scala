package xyz.hyperreal.energize2

import java.time.{Instant, OffsetDateTime}
import java.util.Base64

import org.mindrot.jbcrypt.BCrypt
import xyz.hyperreal.bvm.VM


object AuthorizationFunctionHelpers {
	val CREDENTIALS = "(.*):(.*)"r
	lazy val SCHEME = AUTHORIZATION.getString( "scheme" )
	lazy val EXPIRATION = if (AUTHORIZATION.getIsNull( "expiration" )) Int.MaxValue else AUTHORIZATION.getInt( "expiration" )

	SCHEME match {
		case "Basic"|"Bearer" =>
		case _ => sys.error( """authorization scheme must be "Basic" or "Bearer" """ )
	}

	if (EXPIRATION <= 0)
		sys.error( "authorization expiration value must be positive" )

	def performLogin( vm: VM, user: Long ) = {
		val tokens = vm resource "tokens"
		val token = {
			def gentoken: String = {
				val tok = UtilityFunctions.rndAlpha( vm, 15 )

				tokens.findOption( "token", tok, false ) match {
					case None => tok
					case Some( _ ) => gentoken
				}
			}

			gentoken
		}

		tokens.insert( Map("token" -> token, "created" -> now, "user" -> user).asInstanceOf[OBJ] )
		token
	}

	def auth( req: Map[String, Any] ) =
		req("parse").asInstanceOf[String => (String, Map[String, (String, Map[String, String])])]( "Authorization" ) match {
			case null => None
			case (_, elems) => elems get AuthorizationFunctionHelpers.SCHEME
		}

}

object AuthorizationFunctions {
	def register( vm: VM, json: OBJ ) = {
		val users = vm resource "users"
		val required = users.names.toSet -- Set( "createdTime", "updatedTime", "state", "groups" )

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

		AuthorizationFunctionHelpers.performLogin( vm, users.insert(json1) )
	}

	def login( vm: VM, json: OBJ ) = {
		val required = Set( "email", "password" )

		if ((required -- json.keySet) nonEmpty)
			throw new BadRequestException( "login: missing field(s): " + (required -- json.keySet).mkString(", ") )

		if ((json.keySet -- required) nonEmpty)
			throw new BadRequestException( "register: excess field(s): " + (required -- json.keySet).mkString(", ") )

		val users = vm resource "users"
		val email = json("email")

		def denied = throw new UnauthorizedException( "email or password doesn't match" )

		users.findOption( "email", email, true ) match {
			case None => denied
			case Some( u ) =>
				if (BCrypt.checkpw( json("password").asInstanceOf[String], u("password").asInstanceOf[String] ))
					AuthorizationFunctionHelpers.performLogin( vm, u("_id").asInstanceOf[Long] )
				else
					denied
		}
	}

	def logout( vm: VM, req: Map[String, Any] ) = {
		val access = AuthorizationFunctionHelpers.auth( req )

		if (access isEmpty)
			0
		else
			(vm resource "tokens").deleteValue( "token", access.get )
	}

	def me( vm: VM, req: Map[String, Any] ) = {
		val access = AuthorizationFunctionHelpers.auth( req )

		def barred = throw new UnauthorizedException( "Protected" )

		if (access isEmpty)
			barred

		if (AuthorizationFunctionHelpers.SCHEME == "Basic") {
			val AuthorizationFunctionHelpers.CREDENTIALS(email, password) = new String(Base64.getDecoder.decode(access.get.asInstanceOf[String]))

			(vm resource "users").findOption( "email", email, true ) match {
				case None => barred
				case Some(u) =>
					if (!BCrypt.checkpw(password, u("password").asInstanceOf[String]))
						barred

					u
			}
		} else {
			(vm resource "tokens").findOption( "token", access.get, false ) match {
				case None => barred
				case Some( t ) =>
					if (Instant.now.getEpochSecond - OffsetDateTime.parse(t("created").asInstanceOf[String]).toInstant.getEpochSecond >=
						AuthorizationFunctionHelpers.EXPIRATION)
						throw new ExpiredException( "Protected" )

					t("user").asInstanceOf[OBJ]
			}
		}
	}

	def authorize( vm: VM, group: Option[String], req: Map[String, Any] ) {
		val key = req("query").asInstanceOf[Map[String, String]] get "access_token"

		if (key.isEmpty) {
			val access = AuthorizationFunctionHelpers.auth( req )

			def barred = throw new UnauthorizedException( "Protected" )

			if (access isEmpty)
				barred

			if (AuthorizationFunctionHelpers.SCHEME == "Basic") {
				val AuthorizationFunctionHelpers.CREDENTIALS( email, password ) = new String( Base64.getDecoder.decode( access.get._1 ) )

				(vm resource "users").findOption( "email", email, true ) match {
					case None => barred
					case Some( u ) =>
						if (!BCrypt.checkpw( password, u( "password" ).asInstanceOf[String] ))
							barred

						if (group.nonEmpty && !u( "groups" ).asInstanceOf[List[String]].contains( group.get ))
							barred
				}
			} else {
				(vm resource "tokens").findOption( "token", access.get, false ) match {
					case None => barred
					case Some( t ) =>
						if (group.nonEmpty && !t( "user" ).asInstanceOf[OBJ]( "groups" ).asInstanceOf[List[String]].contains( group.get ))
							barred

						if (Instant.now.getEpochSecond - OffsetDateTime.parse( t("created").asInstanceOf[String] ).toInstant.getEpochSecond >=
							AuthorizationFunctionHelpers.EXPIRATION)
							throw new ExpiredException( "Protected" )
				}
			}
		} else
			access( vm, key )
	}

	def access( vm: VM, key: Option[String] ): Unit =
		if (key.isEmpty || key.get != vm.key)
			throw new ForbiddenException( "wrong or missing access_token" )

}

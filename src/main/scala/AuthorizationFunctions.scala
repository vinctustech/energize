package xyz.hyperreal.energize

import java.time.temporal.ChronoUnit
import java.time.{Instant, OffsetDateTime}
import java.util.Base64

import org.mindrot.jbcrypt.BCrypt


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

	def performLogin( env: Environment, user: Long ) = {
		val tokens = (env get "tokens" get).asInstanceOf[Table]
		val token = {
			def gentoken: String = {
				val tok = UtilityFunctions.rndAlpha( env, 15 )

				QueryFunctions.findOption( env, tokens, "token", tok ) match {
					case None => tok
					case Some( _ ) => gentoken
				}
			}

			gentoken
		}

		CommandFunctions.insert( env, tokens, Map("token" -> token, "created" -> UtilityFunctions.now(env), "user" -> user).asInstanceOf[OBJ] )
		token
	}
}

object AuthorizationFunctions {
	def register( env: Environment, json: OBJ ) = {
		val users = (env get "users" get).asInstanceOf[Table]
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
			}) + ("groups" -> List("user")) + ("createdTime" -> UtilityFunctions.now( env ))

		AuthorizationFunctionHelpers.performLogin( env, CommandFunctions.insert(env, users, json1) )
	}

	def login( env: Environment, json: OBJ ) = {
		val required = Set( "email", "password" )

		if ((required -- json.keySet) nonEmpty)
			throw new BadRequestException( "login: missing field(s): " + (required -- json.keySet).mkString(", ") )

		if ((json.keySet -- required) nonEmpty)
			throw new BadRequestException( "register: excess field(s): " + (required -- json.keySet).mkString(", ") )

		val users = (env get "users" get).asInstanceOf[Table]
		val email = json("email")

		def denied = throw new UnauthorizedException( "email or password doesn't match" )

		QueryFunctions.findOption( env, users, "email", email ) match {
			case None => denied
			case Some( u ) =>
				if (BCrypt.checkpw( json("password").asInstanceOf[String], u("password").asInstanceOf[String] ))
					AuthorizationFunctionHelpers.performLogin( env, u("id").asInstanceOf[Long] )
				else
					denied
		}
	}

	def logout( env: Environment ) = {
		val access = env.variables get (if (AuthorizationFunctionHelpers.SCHEME == "Basic") "$basic" else "$bearer")

		if (access.isEmpty)
			0
		else
			CommandFunctions.deleteValue( env, (env get "tokens" get).asInstanceOf[Table], "token", access.get )
	}

	def authorize( env: Environment, group: String ) {
		val access = env.variables get (if (AuthorizationFunctionHelpers.SCHEME == "Basic") "$basic" else "$bearer")

		def barred = throw new UnauthorizedException( "Protected" )

		if (access isEmpty)
			barred

		if (AuthorizationFunctionHelpers.SCHEME == "Basic") {
			val AuthorizationFunctionHelpers.CREDENTIALS( email, password ) = new String( Base64.getDecoder.decode( access.get.asInstanceOf[String] ) )

			QueryFunctions.findOption( env, (env get "users" get).asInstanceOf[Table], "email", email ) match {
				case None => barred
				case Some( u ) =>
					if (!BCrypt.checkpw( password, u( "password" ).asInstanceOf[String] ) || !u( "groups" ).asInstanceOf[List[String]].contains( group ))
						barred
			}
		} else {
			QueryFunctions.findOption( env, (env get "tokens" get).asInstanceOf[Table], "token", access.get ) match {
				case None => barred
				case Some( t ) =>
					if (Instant.now.getEpochSecond - OffsetDateTime.parse(t("created").asInstanceOf[String]).toInstant.getEpochSecond >=
						AuthorizationFunctionHelpers.EXPIRATION)
						throw new ExpiredException( "Protected" )
			}
		}
	}
}

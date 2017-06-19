package xyz.hyperreal.energize

import org.apache.http.HttpStatus._

import xyz.hyperreal.numbers.ComplexBigInt


object Builtins {
	val constants =
		List(
			"i" -> ComplexBigInt.i,
			"e" -> math.E,
			"pi" -> math.Pi,
			"None" -> None,
			"SC_OK" -> SC_OK,
			"SC_CREATED" -> SC_CREATED,
			"SC_NO_CONTENT" -> SC_NO_CONTENT,
			"SC_BAD_REQUEST" -> SC_NOT_FOUND,
			"SC_NOT_ACCEPTABLE" -> SC_NOT_ACCEPTABLE
		)
	
	def pairs( natives: List[Native] ) = natives map (n => n.name -> n)
	
	val map =
		pairs( Native(QueryFunctions) ) ++
		pairs( Native(CommandFunctions) ) ++
		pairs( Native(ResultFunctions) ) ++
		pairs( Native(UtilityFunctions) ) ++
		pairs( Native(AuthorizationFunctions) ) ++
		pairs( Native(FileFunctions) ) ++
		constants toMap

	def sys =
		Map(
			"version" -> new SystemConstant( VERSION ),
			"entity" -> new SystemVariable
		)

	val routes =
		"""
		|routes <base>/<resource> <authorize>
		|  GET     /id:long                   OkSingleOrNotFound( findID(<resource>, /id, ?fields, None, None, None), /id )
		|  GET     /                          Ok( list(<resource>, ?fields, ?filter, ?order, ?page, ?start, ?limit) )
		|  GET     /size                      Ok( size(<resource>) )
		|  POST    /                          Created( insert(<resource>, $entity) )
		|  PATCH   /id:long                   OkAtLeastOneOrNotFoundId( update(<resource>, /id, $entity, false), /id )
		|  PUT     /id:long                   OkAtLeastOneOrNotFoundId( update(<resource>, /id, $entity, true), /id )
		|  DELETE  /id:long                   OkAtLeastOneOrNotFoundId( delete(<resource>, /id), /id )
		""".stripMargin

	val mtmroutes =
		"""
		|routes <base>/<resource> <authorize>
		|  GET     /id:long/field:            OkSingleOrNotFound( findIDMany(<resource>, /id, /field, ?page, ?start, ?limit), /id )
		|  POST    /id:long/field:            Created( append(<resource>, /id, /field, $entity) )
		|  POST    /sid:long/field:/tid:long  appendIDs( <resource>, /sid, /field, /tid ); NoContent()
		|  DELETE  /id:long/field:            deleteLinks( <resource>, /id, /field, $entity ); NoContent()
		|  DELETE  /id:long/field:/tid:long   OkAtLeastOneOrNotFoundId( deleteLinksID(<resource>, /id, /field, /tid), /id )
		""".stripMargin

	val special =
		"""
		|resource users private
		|  email string unique
		|  createdTime timestamp
		|  updatedTime timestamp
		|  state integer
		|  groups [string]
		|  password string secret
		|
		|routes protected
		|  GET     /users/me                  Ok( me() )
		|
		|table tokens
		|  token string unique
		|  created timestamp
		|  user users
		|
		|routes /meta
		|  DELETE  /res:                      dataResult( res, deleteResource(res) )
		|
		|routes <base>
		|  POST    /login                     Ok( login($entity) )
		|  GET     /logout                    OkAtLeastOneOrNotFound( logout(), "token not found" )
		|  POST    /register                  Created( register($entity) )
		|
		|table _media_
		|  type string
		|  data blob
		|
		|routes
		|  GET /"media"/id:long                Ok( readMedia(/id) )
		""".stripMargin
}

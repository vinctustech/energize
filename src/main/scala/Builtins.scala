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
		constants toMap
	
	val routes =
		"""
		|routes <base>/<resource> <authorize>
		|  GET     /id:long                   OkSingleOrNotFound( "<resource>", findID(<resource>, id, ?fields, None, None, None), id )
		|  GET     /                          Ok( "<resource>", list(<resource>, ?fields, ?filter, ?order, ?page, ?start, ?limit) )
		|  POST    /                          Created( "<resource>", insert(<resource>, json) )
		|  PATCH   /id:long                   OkAtLeastOneOrNotFoundId( update(<resource>, id, json, false), id )
		|  PUT     /id:long                   OkAtLeastOneOrNotFoundId( update(<resource>, id, json, true), id )
		|  DELETE  /id:long                   OkAtLeastOneOrNotFoundId( delete(<resource>, id), id )
		""".stripMargin

	val mtmroutes =
		"""
		|routes <base>/<resource> <authorize>
		|  GET     /id:long/field:            OkSingleOrNotFound( "<resource>", findIDMany(<resource>, id, field, ?page, ?start, ?limit), id )
		|  POST    /id:long/field:            Created( "<resource>", append(<resource>, id, field, json) )
		|  POST    /sid:long/field:/tid:long  appendIDs( <resource>, sid, field, tid ); NoContent()
		|  DELETE  /id:long/field:            deleteLinks( <resource>, id, field, json ); NoContent()
		|  DELETE  /id:long/field:/tid:long   OkAtLeastOneOrNotFoundId( deleteLinksID(<resource>, id, field, tid), id )
		""".stripMargin

	val special =
		"""
		|resource users #protected (admin)
		|  email string unique
		|  createdTime timestamp
		|  updatedTime timestamp
		|  state integer
		|  groups string array
		|  password string secret
		|
		|resource tokens						# should be a table not a resource
		|  token string unique
		|  created timestamp
		|  user users
		|
		|routes /meta
		|  DELETE  /res:                      dataResult( res, deleteResource(res) )
		|
		|routes <base>
		|  POST    /login                     Ok( "login", login(json) )
		|  GET     /logout                    OkAtLeastOneOrNotFound( logout(), "token not found" )
		|  POST    /register                  Created( "registration", register(json) )
		""".stripMargin
}

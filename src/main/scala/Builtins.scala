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
			"SC_TEAPOT" -> 418,
			"SC_OK" -> SC_OK,
			"SC_CREATED" -> SC_CREATED,
			"SC_NO_CONTENT" -> SC_NO_CONTENT,
			"SC_BAD_REQUEST" -> SC_BAD_REQUEST,
			"SC_NOT_FOUND" -> SC_NOT_FOUND,
			"SC_NOT_ACCEPTABLE" -> SC_NOT_ACCEPTABLE
		)
	
	def pairs( natives: List[Native] ) = natives map (n => n.name -> n)
	
	val map =
		pairs( Native(QueryFunctions) ) ++
		pairs( Native(CommandFunctions) ) ++
		pairs( Native(ResultFunctions) ) ++
		pairs( Native(SupportFunctions) ) ++
		constants toMap
	
	val routes =
		"""
		|route <base>/<resource>
		|  GET     /id:long                   dataResult( "<resource>", singleOrNotFound(findID(<resource>, id, ?fields, None, None, None)) )
		|  GET     /                          dataResult( "<resource>", list(<resource>, ?fields, ?filter, ?order, ?page, ?start, ?limit) )
		|  POST    /                          dataResult( "<resource>", insert(<resource>, json) )
		|  PATCH   /id:long                   atLeastOneOrNotFound( update(<resource>, id, json, false) ); null
		|  PUT     /id:long                   atLeastOneOrNotFound( update(<resource>, id, json, true) ); null
		|  DELETE  /id:long                   atLeastOneOrNotFound( delete(<resource>, id) ); null
		""".stripMargin

	val mtmroutes =
		"""
		|route <base>/<resource>
		|  GET     /id:long/field:            dataResult( "<resource>", singleOrNotFound(findIDMany(<resource>, id, field, ?page, ?start, ?limit)) )
		|  POST    /id:long/field:            dataResult( "<resource>", append(<resource>, id, field, json) )
		|  POST    /sid:long/field:/tid:long  appendIDs( <resource>, sid, field, tid )
		|  DELETE  /id:long/field:            deleteLinks( <resource>, id, field, json )
		|  DELETE  /id:long/field:/tid:long   atLeastOneOrNotFound( deleteLinksID(<resource>, id, field, tid) ); null
		""".stripMargin

	//insertLinks(<resource>, id, field, json)

	val special =
		"""
		|route /meta
		|  DELETE  /res:                      dataResult( res, deleteResource(res) )
		|
		|route <auth>
		|  GET     /login                     login()
		|  GET     /logout                    logout()
		|  GET     /register                  register()
		""".stripMargin
}

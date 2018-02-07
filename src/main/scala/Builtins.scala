package xyz.hyperreal.energize2

import org.apache.http.HttpStatus._
import xyz.hyperreal.bvm.Native


object Builtins {
	val constants =
		List(
			"SC_OK" -> SC_OK,
			"SC_CREATED" -> SC_CREATED,
			"SC_NO_CONTENT" -> SC_NO_CONTENT,
			"SC_BAD_REQUEST" -> SC_NOT_FOUND,
			"SC_NOT_ACCEPTABLE" -> SC_NOT_ACCEPTABLE
		)
	val jsconstants =
		List(
			"SC_OK" -> SC_OK,
			"SC_CREATED" -> SC_CREATED,
			"SC_NO_CONTENT" -> SC_NO_CONTENT,
			"SC_BAD_REQUEST" -> SC_NOT_FOUND,
			"SC_NOT_ACCEPTABLE" -> SC_NOT_ACCEPTABLE
		)
	val natives =
		Native( QueryFunctions ) ++
		Native( AggregateFunctions ) ++
		Native( CommandFunctions ) ++
		Native( ResultFunctions ) ++
		Native( UtilityFunctions ) ++
		Native( AuthorizationFunctions ) ++
		Native( FileFunctions )
	val map =
		pairs( natives ) ++ constants toMap
//	val jsmap =
//		pairs( natives ) ++ jsconstants toMap

	def pairs( natives: List[Native] ) = natives map (n => n.name -> n)

	val routes =
		"""
			|routes <base>/<resource> <authorize>
			|  GET     /id:int64    => OkSingleOrNotFound( findID(<resource>, req.path.id, \?req.query.fields, None, None, None), req.path.id )
			|  GET                  => Ok( list(<resource>, \?req.query.fields, \?req.query.filter, \?req.query.order, \?req.query.page, \?req.query.start, \?req.query.limit) )
			|  GET     /count       => Ok( count(<resource>, \?req.query.filter) )
			|  GET     /sum/field:  => Ok( sum(<resource>, \?req.query.filter, req.path.field) )
			|  GET     /avg/field:  => Ok( avg(<resource>, \?req.query.filter, req.path.field) )
			|  GET     /min/field:  => Ok( min(<resource>, \?req.query.filter, req.path.field) )
			|  GET     /max/field:  => Ok( max(<resource>, \?req.query.filter, req.path.field) )
			|  GET     /schema      => Ok( tableSchema(<resource>) )
			|  POST                 => Created( insert(<resource>, req.body) )
			|  PATCH   /id:int64    => OkAtLeastOneOrNotFoundId( update(<resource>, req.path.id, req.body, false), req.path.id )
			|  PUT     /id:int64    => OkAtLeastOneOrNotFoundId( update(<resource>, req.path.id, req.body, true), req.path.id )
			|  DELETE  /id:int64    => OkAtLeastOneOrNotFoundId( delete(<resource>, req.path.id), req.path.id )
			|  DELETE   						=> (deleteMany( <resource>, \?req.query.filter ); NoContent())
		""".stripMargin

	val mtmroutes =
		"""
			|routes <base>/<resource> <authorize>
			|  GET     /id:int64/field:                    => OkSingleOrNotFound( findIDMany(<resource>, req.path.id, req.path.field, \?req.query.page, \?req.query.start, \?req.query.limit), req.path.id )
			|  POST    /id:int64/field:                    => Created( append(<resource>, req.path.id, req.path.field, req.body) )
			|  POST    /sid:int64/field:/target/tid:int64  => (appendIDs( <resource>, req.path.sid, req.path.field, req.path.tid ); NoContent())
			|  DELETE  /id:int64/field:                    => (deleteLinks( <resource>, req.path.id, req.path.field, req.body ); NoContent())
			|  DELETE  /id:int64/field:/target/tid:int64   => OkAtLeastOneOrNotFoundId( deleteLinksID(<resource>, req.path.id, req.path.field, req.path.tid), req.path.id )
		""".stripMargin

	val arrayroutes =
		"""
			|routes <base>/<resource> <authorize>
			|  POST    /id:int64/field:/idx:integer  => (arrayInsert( <resource>, req.path.id, req.path.field, req.path.idx, req.body ); NoContent())
			|  PUT     /id:int64/field:/idx:integer  => (arrayUpdate( <resource>, req.path.id, req.path.field, req.path.idx, req.body ); NoContent())
			|  DELETE  /id:int64/field:/idx:integer  => (arrayDelete( <resource>, req.path.id, req.path.field, req.path.idx ); NoContent())
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
			|  GET     /users/me  => Ok( me() )
			|
			|table tokens
			|  token string unique
			|  created timestamp
			|  user users
			|
			|routes /meta private
			|  POST    /resourse:/field:                 => Ok( createField(req.path.resourse, req.path.field, req.body) )
			|  PUT     /resourse:/field:/newname:        => Ok( renameField(req.path.resourse, req.path.field, req.path.newname) )
			|  DELETE  /resourse:/field:                 => Ok( deleteField(req.path.resourse, req.path.field) )
			|  DELETE  /resourse:                        => Ok( deleteResource(req.path.resourse) )
			|  GET     /schema												   => Ok( databaseSchema() )
			|
			|routes <base>
			|  POST    /login        => Ok( login(req.body) )
			|  GET     /logout       => OkAtLeastOneOrNotFound( logout(), "token not found" )
			|  POST    /register     => Created( register(req.body) )
			|
			|table _media_
			|  type string
			|  "data" blob(urlchars)
			|
			|routes
			|  GET  /media/id:int64  => Ok( readMedia(req.path.id) )
		""".stripMargin
}

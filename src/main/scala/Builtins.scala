package xyz.hyperreal.energize

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
		Native( ResultFunctions ) ++
		Native( UtilityFunctions ) ++
		Native( AuthenticationFunctions ) ++
		Native( FileFunctions )
	val map =
		pairs( natives ) ++ constants toMap
	//	val jsmap =
	//		pairs( natives ) ++ jsconstants toMap

	def pairs( natives: List[Native] ) = natives map (n => n.name -> n)

	def routes( base: String, resource: String, authorize: String ) =
		s"""
			|routes $base/$resource $authorize
			|  GET     /id:int64    => OkSingleOrNotFound( res, $resource.findID(req.params.id, \\?req.query.fields, None, None, None), req.params.id )
			|  GET                  => Ok( res, $resource.list(\\?req.query.fields, \\?req.query.filter, \\?req.query.order, \\?req.query.page, \\?req.query.start, \\?req.query.limit) )
			|  GET     /count       => Ok( res, $resource.count(\\?req.query.filter) )
			|  GET     /sum/field:  => Ok( res, $resource.sum(\\?req.query.filter, req.params.field) )
			|  GET     /avg/field:  => Ok( res, $resource.avg(\\?req.query.filter, req.params.field) )
			|  GET     /min/field:  => Ok( res, $resource.min(\\?req.query.filter, req.params.field) )
			|  GET     /max/field:  => Ok( res, $resource.max(\\?req.query.filter, req.params.field) )
			|  GET     /schema      => Ok( res, resourceSchema($resource) )
			|  POST                 => Created( res, $resource.insert(req.body) )
			|  PATCH   /id:int64    => OkAtLeastOneOrNotFoundId( res, $resource.update(req.params.id, req.body, false), req.params.id )
			|  PUT     /id:int64    => OkAtLeastOneOrNotFoundId( res, $resource.update(req.params.id, req.body, true), req.params.id )
			|  DELETE  /id:int64    => OkAtLeastOneOrNotFoundId( res, $resource.delete(req.params.id), req.params.id )
			|  DELETE   						=>
			|    deleteMany( $resource, \\?req.query.filter )
			|    NoContent( res )
		""".stripMargin

	def mtmroutes( base: String, resource: String, authorize: String ) =
		s"""
			|routes $base/$resource $authorize
			|  GET     /id:int64/field:                    => OkSingleOrNotFound( res, $resource.findIDMany(req.params.id, req.params.field, \\?req.query.page, \\?req.query.start, \\?req.query.limit), req.params.id )
			|  POST    /id:int64/field:                    => Created( res, $resource.append(req.params.id, req.params.field, req.body) )
			|  POST    /sid:int64/field:/target/tid:int64  =>
			|    $resource.appendIDs( req.params.sid, req.params.field, req.params.tid )
			|    NoContent( res )
			|  DELETE  /id:int64/field:                    =>
			|    deleteLinks( $resource, req.params.id, req.params.field, req.body )
			|    NoContent( res )
			|  DELETE  /id:int64/field:/target/tid:int64   => OkAtLeastOneOrNotFoundId( res, deleteLinksID($resource, req.params.id, req.params.field, req.params.tid), req.params.id )
		""".stripMargin

	def arrayroutes( base: String, resource: String, authorize: String ) =
		s"""
			|routes $base/$resource $authorize
			|  POST    /id:int64/field:/idx:integer  =>
			|    arrayInsert( $resource, req.params.id, req.params.field, req.params.idx, req.body )
			|    NoContent( res )
			|  PUT     /id:int64/field:/idx:integer  =>
			|    arrayUpdate( $resource, req.params.id, req.params.field, req.params.idx, req.body )
			|    NoContent( res )
			|  DELETE  /id:int64/field:/idx:integer  =>
			|    arrayDelete( $resource, req.params.id, req.params.field, req.params.idx )
			|    NoContent( res )
		""".stripMargin

	def special( base: String ) =
		s"""
			|resource users private
			|  email string unique
			|  createdTime timestamp
			|  updatedTime timestamp
			|  state integer
			|  groups [string]
			|  password string secret
			|
			|routes
			|  GET     /users/me  => Ok( res, authorization(req) )
			|
			|routes /meta private
			|  POST    /resourse:/field:                 => Ok( res, createField(req.params.resourse, req.params.field, req.body) )
			|  PUT     /resourse:/field:/newname:        => Ok( res, renameField(req.params.resourse, req.params.field, req.params.newname) )
			|  DELETE  /resourse:/field:                 => Ok( res, deleteField(req.params.resourse, req.params.field) )
			|  DELETE  /resourse:                        => Ok( res, deleteResource(req.params.resourse) )
			|  GET     /schema												   => Ok( res, databaseSchema() )
			|
			|routes $base
			|  POST    /login        => Ok( res, login(req.body) )
			|  POST    /register     => Created( res, register(req.body) )
			|
			|table _media_
			|  type string
			|  "data" blob(urlchars)
			|
			|routes
			|  GET  /media/id:int64  => Ok( res, req.proc.readMedia(req.params.id) )
		""".stripMargin
}

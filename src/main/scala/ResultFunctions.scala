package xyz.hyperreal.energize

import org.apache.http.HttpStatus._


object ResultFunctions {
	def Data( env: Environment, typ: String, code: Int, data: Any ) = (code, Map( "data" -> data ))
	def Ok( env: Environment, typ: String, data: Any ) = Data( env, typ, SC_OK, data )
	def Created( env: Environment, typ: String, data: Any ) = Data( env, typ, SC_CREATED, data )
	def NoContent( env: Environment ) = (SC_NO_CONTENT, null)

	def Error( env: Environment, code: Int, error: String ) = (code, Map( "error" -> error ))
	def BadRequest( env: Environment, error: String ) = Error( env, SC_BAD_REQUEST, error )
	def NotFound( env: Environment, error: String ) = Error( env, SC_NOT_FOUND, error )
	def NotAcceptable( env: Environment, error: String ) = Error( env, SC_NOT_ACCEPTABLE, error )
	def Conflict( env: Environment, error: String ) = Error( env, SC_CONFLICT, error )
	def Forbidden( env: Environment, error: String ) = Error( env, SC_FORBIDDEN, error )
	def Unauthorized( env: Environment, realm: String ) = (SC_UNAUTHORIZED, realm)

	def OkSingleOrNotFound( env: Environment, typ: String, list: List[Any], id: Long ) =
		list match {
			case Nil => NotFound( env, s"id $id not found" )
			case List( h ) => Ok( env, typ, h )
			case _ => NotAcceptable( env, "more than one item in result list" )
		}

	def OkAtLeastOneOrNotFound( env: Environment, typ: String, count: Int, id: Long ) =
		count match {
			case 0 => NotFound( env, s"id $id not found" )
			case _ => NoContent( env )
		}

	//	def JSONAPIdataResult( env: Env, typ: String, data: Any ) =
}

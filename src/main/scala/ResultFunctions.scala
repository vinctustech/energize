package xyz.hyperreal.energize

import org.apache.http.HttpStatus._


object ResultFunctions {
	def Data( env: Environment, code: Int, data: Any ) = (code, "application/json", Map( "data" -> data ))
	def Ok( env: Environment, data: Any ) =
		data match {
			case (mime, data) => (SC_OK, mime, data)
			case _ => Data( env, SC_OK, data )
		}
	def Created( env: Environment, data: Any ) = Data( env, SC_CREATED, data )
	def NoContent( env: Environment ) = (SC_NO_CONTENT, null, null)

	def Error( env: Environment, code: Int, error: String ) = (code, "application/json", Map( "error" -> error ))
	def BadRequest( env: Environment, error: String ) = Error( env, SC_BAD_REQUEST, error )
	def NotFound( env: Environment, error: String ) = Error( env, SC_NOT_FOUND, error )
	def NotAcceptable( env: Environment, error: String ) = Error( env, SC_NOT_ACCEPTABLE, error )
	def Conflict( env: Environment, error: String ) = Error( env, SC_CONFLICT, error )
	def Forbidden( env: Environment, error: String ) = Error( env, SC_FORBIDDEN, error )
	def Unauthorized( env: Environment, error: String ) = (SC_UNAUTHORIZED, null, error)

	def OkSingleOrNotFound( env: Environment, list: List[Any], id: Long ) =
		list match {
			case Nil => NotFound( env, s"id $id not found" )
			case List( h ) => Ok( env, h )
			case _ => NotAcceptable( env, "more than one item in result list" )
		}

	def OkAtLeastOneOrNotFoundId( env: Environment, count: Int, id: Long ) = OkAtLeastOneOrNotFound( env, count, s"id $id not found" )

	def OkAtLeastOneOrNotFound( env: Environment, count: Int, error: String ) =
		count match {
			case 0 => NotFound( env, error )
			case _ => NoContent( env )
		}

	//	def JSONAPIdataResult( env: Env, typ: String, data: Any ) =
}

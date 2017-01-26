package xyz.hyperreal.energize

import org.apache.http.HttpStatus._


object ResultFunctions {
	def Ok( env: Env, typ: String, data: Any ) = (SC_OK, Map( "data" -> data ))
	def Created( env: Env, typ: String, data: Any ) = (SC_CREATED, Map( "data" -> data ))
	def NoContent( env: Env ) = (SC_NO_CONTENT, null)

	def BadRequest( env: Env, error: String ) = (SC_BAD_REQUEST, Map( "error" -> error ))
	def NotFound( env: Env, error: String ) = (SC_NOT_FOUND, Map( "error" -> error ))
	def NotAcceptable( env: Env, error: String ) = (SC_NOT_ACCEPTABLE, Map( "error" -> error ))

	def OkSingleOrNotFound( env: Env, typ: String, list: List[Any], id: Long ) =
		list match {
			case Nil => NotFound( env, s"id $id not found" )
			case List( h ) => Ok( env, typ, h )
			case _ => NotAcceptable( env, "more than one item in result list" )
		}

	def OkAtLeastOneOrNotFound( env: Env, typ: String, count: Int, id: Long ) =
		count match {
			case 0 => NotFound( env, s"id $id not found" )
			case _ => NoContent( env )
		}

	//	def JSONAPIdataResult( env: Env, typ: String, data: Any ) =
}

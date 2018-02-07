package xyz.hyperreal.energize2

import org.apache.http.HttpStatus._
import xyz.hyperreal.bvm.VM


object ResultFunctions {
	def Data( vm: VM, code: Int, data: Any ) = (code, "application/json", Map( "data" -> data ))
	def Ok( vm: VM, data: Any ) =
		data match {
			case (mime, content) => (SC_OK, mime, content)
			case _ => Data( vm, SC_OK, data )
		}
	def Created( vm: VM, data: Any ) = Data( vm, SC_CREATED, data )
	def NoContent( vm: VM ) = (SC_NO_CONTENT, null, null)

	def Error( vm: VM, code: Int, error: String ) = (code, "application/json", Map( "error" -> error ))
	def BadRequest( vm: VM, error: String ) = Error( vm, SC_BAD_REQUEST, error )
	def NotFound( vm: VM, error: String ) = Error( vm, SC_NOT_FOUND, error )
	def NotAcceptable( vm: VM, error: String ) = Error( vm, SC_NOT_ACCEPTABLE, error )
	def Conflict( vm: VM, error: String ) = Error( vm, SC_CONFLICT, error )
	def Forbidden( vm: VM, error: String ) = Error( vm, SC_FORBIDDEN, error )
	def Unauthorized( vm: VM, error: String ) = (SC_UNAUTHORIZED, null, error)

	def OkSingleOrNotFound( vm: VM, list: List[Any], id: Long ) =
		list match {
			case Nil => NotFound( vm, s"id $id not found" )
			case List( h ) => Ok( vm, h )
			case _ => NotAcceptable( vm, "more than one item in result list" )
		}

	def OkAtLeastOneOrNotFoundId( vm: VM, count: Int, id: Long ) = OkAtLeastOneOrNotFound( vm, count, s"id $id not found" )

	def OkAtLeastOneOrNotFound( vm: VM, count: Int, error: String ) =
		count match {
			case 0 => NotFound( vm, error )
			case _ => NoContent( vm )
		}

	//	def JSONAPIdataResult( vm: Env, typ: String, data: Any ) =
}

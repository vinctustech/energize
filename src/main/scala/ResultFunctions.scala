package xyz.hyperreal.energize

import org.apache.http.HttpStatus._
import xyz.hyperreal.bvm.VM


object ResultFunctions {
	def Data( vm: VM, res: Response, code: Int, data: Any ) = res.status( code ).send( Map("data" -> data) )

	def Ok( vm: VM, res: Response, data: Any ) =
		data match {
			case (mime: String, content: AnyRef) => res.`type`( mime ).send( content )
			case _ => Data( vm, res, SC_OK, data )
		}

	def Created( vm: VM, res: Response, data: Any ) = Data( vm, res, SC_CREATED, data )
	def NoContent( vm: VM, res: Response ) = res.sendStatus( SC_NO_CONTENT )

	def Error( vm: VM, res: Response, code: Int, error: String ) = res.status( code ).send( Map("error" -> error) )

	def BadRequest( vm: VM, res: Response, error: String ) = Error( vm, res, SC_BAD_REQUEST, error )
	def NotFound( vm: VM, res: Response, error: String ) = Error( vm, res, SC_NOT_FOUND, error )
	def NotAcceptable( vm: VM, res: Response, error: String ) = Error( vm, res, SC_NOT_ACCEPTABLE, error )
	def Conflict( vm: VM, res: Response, error: String ) = Error( vm, res, SC_CONFLICT, error )
	def Forbidden( vm: VM, res: Response, error: String ) = Error( vm, res, SC_FORBIDDEN, error )
	def Unauthorized( vm: VM, res: Response, error: String ) = Error( vm, res, SC_UNAUTHORIZED, error )
	def InternalServerError( vm: VM, res: Response, error: String ) = Error( vm, res, SC_INTERNAL_SERVER_ERROR, error )

	def OkSingleOrNotFound( vm: VM, res: Response, list: List[Any], id: Long ) =
		list match {
			case Nil => NotFound( vm, res, s"id $id not found" )
			case List( h ) => Ok( vm, res, h )
			case _ => NotAcceptable( vm, res, "more than one item in result list" )
		}

	def OkAtLeastOneOrNotFoundId( vm: VM, res: Response, count: Int, id: Long ) = OkAtLeastOneOrNotFound( vm, res, count, s"id $id not found" )

	def OkAtLeastOneOrNotFound( vm: VM, res: Response, count: Int, error: String ) =
		count match {
			case 0 => NotFound( vm, res, error )
			case _ => NoContent( vm, res )
		}

	//	def JSONAPIdataResult( vm: Env, typ: String, data: Any ) =
}

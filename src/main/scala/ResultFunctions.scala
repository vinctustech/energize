package xyz.hyperreal.cras


object ResultFunctions {
	def dataResult( env: Env, typ: String, data: Any ) = Map( "status" -> "ok", "data" -> data )
	
	def errorResult( env: Env, error: String ) = Map( "status" -> "error", "error" -> error )
	
//	def JSONAPIdataResult( env: Env, typ: String, data: Any ) = 
}
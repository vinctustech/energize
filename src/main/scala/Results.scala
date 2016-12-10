package xyz.hyperreal.cras


object Results {
	def OK( env: Env, typ: String, data: Any ) = Map( "status" -> "ok", "data" -> data )
	
	def Error( env: Env, typ: String, error: String ) = Map( "status" -> "error", "error" -> error )
}
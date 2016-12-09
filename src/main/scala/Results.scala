package xyz.hyperreal.cras


object Results {
	def OK( env: Env, data: Any ) = Map( "status" -> "ok", "data" -> data )
	
	def Error( env: Env, error: String ) = Map( "status" -> "error", "error" -> error )
}
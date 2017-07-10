package xyz.hyperreal.energize

import jdk.nashorn.api.scripting.AbstractJSObject


class RequestJSObject( env: Environment ) extends AbstractJSObject {
	lazy val path =
		new AbstractJSObject {
			override def getMember( name: String ) = env.pathMap get name
		}
	lazy val query =
		new AbstractJSObject {
			override def getMember( name: String ) = env.queryMap get name
		}

	override def getMember( name: String ) =
		name match {
			case "body" => env.sbindings( "entity" ).value
			case "path" => path
			case "query" => query
		}
}

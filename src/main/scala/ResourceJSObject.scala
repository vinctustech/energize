package xyz.hyperreal.energize

import jdk.nashorn.api.scripting.AbstractJSObject


class ResourceJSObject( env: Environment, resource: Table ) extends AbstractJSObject {

	override def getMember( name: String ) =
		name match {
			case "size" => QueryFunctions.size( env, resource )
		}

}

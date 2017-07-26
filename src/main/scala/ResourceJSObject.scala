package xyz.hyperreal.energize

import jdk.nashorn.api.scripting.AbstractJSObject


class ResourceJSObject( env: Environment, resource: Table ) extends AbstractJSObject {

	override def getMember( name: String ) =
		name match {
			case "size" => AggregateFunctions.count( env, resource, None )//todo: this should also be a function that allows filtering
		}

}

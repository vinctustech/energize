//@
package xyz.hyperreal.energize

import org.apache.http.client.fluent.Request
import xyz.hyperreal.json.DefaultJSONReader


object ClientFunctions {

  def requestJSON( vm: VM, url: String ): Any = {
    DefaultJSONReader.fromString( Request.Get("http://www.worldclockapi.com/api/json/est/now" )
      .connectTimeout(1000)
      .socketTimeout(1000)
      .execute.returnContent.asString )

}
//@
package xyz.hyperreal.energize

import org.apache.http.client.fluent.Request

import xyz.hyperreal.bvm.VM
import xyz.hyperreal.json.DefaultJSONReader


object ClientFunctions {

  def requestJSON( vm: VM, url: String ): Any =
    DefaultJSONReader.fromString(
      Request.Get(url)
        .addHeader( "Accept", "application/json" )
        .connectTimeout(1000)
        .socketTimeout(1000)
        .execute.returnContent.asString )

}
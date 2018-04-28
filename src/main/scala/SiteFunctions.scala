//@
package xyz.hyperreal.energize

import java.io.{File, PrintStream}

import org.apache.http.HttpStatus._
import xyz.hyperreal.liquescent._
import xyz.hyperreal.json.DefaultJSONReader
import xyz.hyperreal.bvm.VM

import scala.collection.mutable


object SiteFunctionHelpters {

  val apiTag =
    new Tag( "api" ) {
      def apply( vars: mutable.Map[String, Any], out: PrintStream, args: List[Any], context: AnyRef ) =
        args match {
          case List( variable: String, route: String ) =>
            val (sc, mt, json) = context.asInstanceOf[VM].process( "GET", route, null, null, null )

            if (sc == SC_OK)
              vars(variable) = DefaultJSONReader.fromString( json.toString )
            else
              sys.error( s"error requesting $route: $sc" )
        }
    }

}

object SiteFunctions {

  def render( vm: VM, input: File, output: File, assigns: Map[String, String] ): Unit = {
    val out = new PrintStream( output )

    new Interpreter( StandardFilters.map, Tag(SiteFunctionHelpters.apiTag), assigns, vm ).
      perform( LiquescentParser.parse(io.Source.fromFile(input)), out )
    out.close
  }

  def serve( vm: VM, res: Response, path: String, query: Map[String, String], root: String ) = {
		val file = {
			val f = new File( root, path )	//todo: URLDecoder.decode(path, "UTF-8") ???

			if (f isDirectory) {
				val liquid = new File( f, "index.liquid" )
				val index = new File( f, "index.html" )

				if (liquid.exists)
				  render( vm, liquid, index, query )

				if (index.exists)
					index
				else
					f
			} else if (f.getName endsWith ".liquid") {
        val out = new File( f.getPath.substring(0, f.getPath.length - 7) + ".html" )

        if (f.exists && f.canRead) {
          render( vm, f, out, query )
          out
        } else
          f
      } else if (f.getName matches """.*\.[^.]+""")
				f
      else {
        val in = new File( f.getPath + ".liquid" )
        val out = new File( f.getPath + ".html" )

        if (in.exists && in.canRead) {
          render( vm, in, out, query )
          out
        } else
          in
      }
		}

    // if file is a directory, render directory (add new parameter to serve())

		if (!file.exists || file.isDirectory) {
			res.status( SC_NOT_FOUND ).send( s"<html><body><h1>$file not found</h1></body></html>" ).`type`( "text/html" )
		} else if (!file.canRead) {
			res.status( SC_FORBIDDEN ).send( "<html><body><h1>Access denied</h1></body></html>" ).`type`( "text/html" )
		} else
			res.status( SC_OK ).send( file ).`type`( contentTypeFromExtension(file.getName) )
	}

}

/*
						val file = {
							val f = new File(docroot, )

							if (f isDirectory) {
								val index = new File(f, "index.html")

								if (index.exists)
									index
								else
									f
							} else
								f
						}

						if (!file.exists) {
							response.setStatusCode( SC_NOT_FOUND )

							val entity = new NStringEntity(
								s"<html><body><h1>$target not found</h1></body></html>", `text/html`)

							response.setEntity( entity )
						} else if (!file.canRead) {
							response.setStatusCode( SC_FORBIDDEN )

							val entity = new NStringEntity(
								"<html><body><h1>Access denied</h1></body></html>", `text/html`)

							response.setEntity( entity )
						} else if (file.isDirectory) {
							val path =
								file.getAbsolutePath match {
									case p if p endsWith "/" => p dropRight 1
									case p => p
								}
							val parent =
								file.getParent match {
									case null => path
									case p => p
								}
							val files = file.listFiles.toList
							val index =
								<html>
									<head>
										<title>Index of
											{path}
										</title>
									</head>
									<body>
										<h1>Index of
											{path}
										</h1>
										<table>
											<tr>
												<th valign="top">
													<img src="/icons/blank.gif" alt="[ICO]"/>
												</th>
												<th>
													<a href="?C=N;O=D">Name</a>
												</th>
												<th>
													<a href="?C=M;O=A">Last modified</a>
												</th>
												<th>
													<a href="?C=S;O=A">Size</a>
												</th>
												<th>
													<a href="?C=D;O=A">Description</a>
												</th>
											</tr>
											<tr>
												<td valign="top">
													<img src="/icons/back.gif" alt="[PARENTDIR]"/>
												</td>
												<td>
													<a href={parent}>Parent Directory</a>
												</td> <td>
												&nbsp;
											</td>
												<td align="right">-</td>
												<td>
													&nbsp;
												</td>
											</tr>{files map (f =>
											<tr>
												<td valign="top">
													<img src={f.getName} alt="[DIR]"/>
												</td>
												<td>
													<a href={f.getName}>
														{f.getName}
													</a>
												</td>
												<td align="right">
													{MODIFIED.format(f.lastModified)}
												</td>
												<td align="right">-</td>
												<td>
													&nbsp;
												</td>
											</tr>)}
										</table>
									</body>
								</html>
							val entity = new NStringEntity("<!DOCTYPE html>\n" + index, `text/html`)

							response.setStatusCode(SC_OK)
							response.setEntity(entity)
						} else {
							val EXTENSION(ext) = file.getName.toLowerCase
							val contentType =
								ext match {
									case "html"|"htm" => `text/html`
									case "jpeg"|"jpg" => ContentType.create("image/jpeg")
									case "css" => ContentType.create("text/css")
									case "js" => ContentType.create("application/javascript")
									case "json" => `application/json`
								}

							response.setStatusCode(SC_OK)

							val body = new NFileEntity(file, contentType)

							response.setEntity(body)
						}

 */
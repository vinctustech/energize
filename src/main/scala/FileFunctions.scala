package xyz.hyperreal.energize

import java.io.File
import java.nio.file.{Files, Paths}

import xyz.hyperreal.bvm.VM


object FileFunctions {

	def readFileBase64( vm: VM, file: String ) = bytes2base64( Files.readAllBytes(Paths.get(file)) )

	def readAsDataURL( vm: VM, file: String, mediaType: String ) = s"data:$mediaType;base64,${readFileBase64( vm, file )}"

}

/*
						val file = {
							val f = new File(docroot, URLDecoder.decode(target, "UTF-8"))

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
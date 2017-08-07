package xyz.hyperreal.energize

import java.nio.file.{Paths, Files}


object FileFunctions {

	def readFileBase64( env: Environment, file: String ) = bytes2base64( Files.readAllBytes(Paths.get(file)) )

	def readAsDataURL( env: Environment, file: String, mediaType: String ) = "data:image/jpeg;base64," + readFileBase64( env, file )
}
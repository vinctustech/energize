package xyz.hyperreal.energize

import java.nio.file.{Paths, Files}


object FileFunctions {

	def readFileBase64( env: Environment, filename: String ) = bytes2base64( Files.readAllBytes(Paths.get(filename)) )

}
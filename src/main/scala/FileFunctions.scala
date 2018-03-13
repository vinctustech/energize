package xyz.hyperreal.energize2

import java.nio.file.{Files, Paths}

import xyz.hyperreal.bvm.VM


object FileFunctions {

	def readFileBase64( vm: VM, file: String ) = bytes2base64( Files.readAllBytes(Paths.get(file)) )

	def readAsDataURL( vm: VM, file: String, mediaType: String ) = s"data:$mediaType;base64,${readFileBase64( vm, file )}"
}
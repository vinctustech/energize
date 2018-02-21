//@
package xyz.hyperreal.energize2

import org.apache.http._
import org.apache.http.params.HttpParams


object Main extends App {

//  val program =
//    """
//      |write( 'asdf' )
//    """.stripMargin
//
//  run( program, "asdf" )

  val (c, s, d) = Test.dbconnect
  val key = AUTHORIZATION.getString( "key" )
  val src =
    """
			|resource r
			|  a int
			|
      |insert( r, {a: 3} )
			|insert( r, {a: 4} )
    """.trim.stripMargin
  val (pro, _) = Definition.define( src, c, s, d, key )
  val message =
    new HttpMessage {
      override def setHeaders(headers: Array[Header]): Unit = ???
      override def headerIterator(): HeaderIterator = ???
      override def headerIterator(name: String): HeaderIterator = ???
      override def addHeader(header: Header): Unit = ???
      override def addHeader(name: String, value: String): Unit = ???
      override def getHeaders(name: String): Array[Header] = ???
      override def getFirstHeader(name: String): Header =
        name match {
          case "Host" =>
            new Header {
              override def getElements: Array[HeaderElement] = ???
              override def getName: String = "Host"
              override def getValue: String = "abc.com"
            }
          case _ => null
        }
      override def getLastHeader(name: String): Header = ???
      override def getAllHeaders: Array[Header] = ???
      override def removeHeaders(name: String): Unit = ???
      override def setHeader(header: Header): Unit = ???
      override def setHeader(name: String, value: String): Unit = ???
      override def removeHeader(header: Header): Unit = ???
      override def getProtocolVersion: ProtocolVersion = ???
      override def containsHeader(name: String): Boolean = ???
      override def getParams: HttpParams = ???
      override def setParams(params: HttpParams): Unit = ???
   }
  println( pro.process( "GET", "/r/sum/b", new HttpComponentsMessageHeaders(message), null, null ) )

  //  val parser = new FunLParser
//  val ast = parser.parseFromString( program, parser.source ).asInstanceOf[AST]
//  val compiler = new Compiler( Predef.constants ++ Predef.natives, Predef.sysvars, Predef.macros, comments = true )
//  val code = compiler.compile( ast )
//  val vm = new VM( code, Array(), false, true, new AnyRef {def a( x: Int ) = 123} )

  //	println( code.functions, code.variables )
  //	println( ast )
  //	println( code.code.zipWithIndex map {case (c, i) => i + "\t" + c} mkString "\n" )
  //	vm.trace = true
  //	vm.limit = 300
  //	println( vm.call( 2, List(3) ) )
//  vm.execute
//  println( vm )
  //	println( vm.call(vm.global(0), List(5, 7)) )
  //	println( vm.call(code.constants("array"), List(3)) )

}
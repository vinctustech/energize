//@
package xyz.hyperreal.energize

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
  val key = AUTHENTICATION.getString( "key" )
  val src =
//    """
//      |enum e [a, b, c]
//      |
//			|resource r
//			|  f e
//			|
//      |r.insert( {f: d} )
//      |write( r.list(None, None, None, None, None, None) )
//    """.trim.stripMargin
			"""
				|resource test
				|	identifier uuid
			""".trim.stripMargin
  val pro = Definition.define( src, c, s, d, key )

  println( pro.process( "POST", "/test", null, """{identifier: "a94cae4a-8bdf-49f3-849b-e7d338f4400a"}""", null ) )
  println( pro.process( "GET", "/test", null, null, null ) )

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
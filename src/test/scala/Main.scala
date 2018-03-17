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
				|resource customers
				|	CustomerName string indexed
				|	ContactName string indexed
				|	Address string
				|	City string indexed
				|	PostalCode string indexed
				|	Country string indexed
				|
				|if customers.count( None ) == 0
				|	customers.batchInsert( [
				|	 ["Alfreds Futterkiste",                "Maria Anders",       "Obere Str. 57",                 "Berlin",      "12209",    "Germany"],
				|	 ["Ana Trujillo Emparedados y helados", "Ana Trujillo",       "Avda. de la Constitución 2222", "México D.F.", "05021",    "Mexico"],
				|	 ["Antonio Moreno Taquería",            "Antonio Moreno",     "Mataderos 2312",                "México D.F.", "05023",    "Mexico"],
				|	 ["Around the Horn",                    "Thomas Hardy",       "120 Hanover Sq.",               "London",      "WA1 1DP",  "UK"],
				|	 ["Berglunds snabbköp",                 "Christina Berglund", "Berguvsvägen 8",                "Luleå",       "S-958 22", "Sweden"]], false )
			""".trim.stripMargin
  val pro = Definition.define( src, c, s, d, key )

//  println( pro.process( "GET", "/r", null, null, null ) )

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
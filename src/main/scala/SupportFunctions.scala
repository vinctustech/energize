package xyz.hyperreal.cras

import xyz.hyperreal.table.TextTable


object SupportFunctions {
	
	def print( env: Env, o: Any ) = println( o )
	
	def toInt( env: Env, v: String ) = v.toInt
	
	def toLong( env: Env, v: String ) = v.toLong
	
	def toString( env: Env, o: Any ) = o.toString
	
	def singleOrNotFound( env: Env, list: List[Any] ) =
		list.length match {
			case 0 => throw new CrasNotFoundException
			case 1 => list.head
			case _ => throw new CrasErrorException( "more than one item in list" )
		}
	
	def atLeastOneOrNotFound( env: Env, count: Int ) =
		count match {
			case 0 => throw new CrasNotFoundException
			case _ => count
		}
		
	def eval( env: Env, expr: String ) = env.evaluate( expr )
	
	def rndPrintable( env: Env, len: Int ) = List.fill( len )( util.Random.nextPrintableChar ).mkString
	
	def rndAlpha( env: Env, len: Int ) = List.fill( len )( (util.Random.nextInt('z' - 'a') + 'a').toChar ).mkString
	
	def rndInt( env: Env, low: Int, high: Int ) = util.Random.nextInt( high - low ) + low


	def show( env: Env, tab: String ) = Console.print( TextTable(env.statement.executeQuery(s"select * from $tab")) )

	def associate( env: Env, src: Table, sfield: String, svalue: AnyRef, dst: Table, dfield: String, dvalue: AnyRef ) = {
		val sobj = QueryFunctions.findOne( env, src, sfield, svalue )( "id" )
		val dobj = QueryFunctions.findOne( env, dst, dfield, dvalue )( "id" )

		env.statement.executeUpdate( s"insert into ${src.name}$$${dst.name} values ('$sobj', '$dobj')" )
		//		insert( env, env.tables( env.db.desensitize(  ) ),
		//			Map(
		//				src.name + "$id" -> QueryFunctions.findOne( env, src, sfield, svalue )( "id" ),
		//				dst.name + "$id" -> QueryFunctions.findOne( env, dst, dfield, dvalue )( "id" ) ) )
	}
}
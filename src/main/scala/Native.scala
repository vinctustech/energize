package xyz.hyperreal.cras

import collection.mutable.{LinkedHashMap, HashMap, ListBuffer}


object Builtins {
	val list = List( QueryNative, InsertNative, UpdateNative, CommandNative, SingleOrNotFoundNative, AtLeastOneOrNotFoundNative )
	
	def map = list map (n => (n.name -> n)) toMap
}

abstract class Native( val name: String ) extends ((List[Any], Env) => Any) {
	val argc: Int
	
	def apply( args: List[Any], env: Env ): Any
	
	override def toString = name + "/" + argc
}

object QueryNative extends Native( "query" ) {
	val argc = 1
	
	def apply( args: List[Any], env: Env ) = {
		val res = env.statement.executeQuery( args.head.asInstanceOf[String] )
		val list = new ListBuffer[Map[String, Any]]
		val md = res.getMetaData
		val count = md.getColumnCount
		
		while (res.next)
			list += Map( (for (i <- 1 to count) yield (md.getColumnName(i), res.getObject(i))): _* )
		
		list.toList
	}
}

object InsertNative extends Native( "insert" ) {
	val argc = 2
	
	def apply( args: List[Any], env: Env ) = {
		val t = args.head.asInstanceOf[Table]
		val j = args.last.asInstanceOf[Map[String, Any]]
		val com = new StringBuilder( "insert into " )
		val last = t.names.last
		
		com ++= t.name
		com ++= t.names.mkString( "(", ", ", ") values(" )
		
		for (c <- t.names) {
			j get c match {
				case None => com ++= "null"
				case Some( v ) =>
					if (t.columns(c).typ == StringType) {
						com += '\''
						com ++= String.valueOf( v )
						com += '\''
					} else
						com ++= String.valueOf( v )
			}
			
			if (c != last)
				com ++= ", "
		}
		
		com += ')'				
		env.statement.executeUpdate( com.toString )
	}
}

object UpdateNative extends Native( "update" ) {
	val argc = 4
	
	def apply( args: List[Any], env: Env ) = {
		val t = args.head.asInstanceOf[Table]
		val j = args(1).asInstanceOf[Map[String, Any]]
		val idv = args(2).asInstanceOf[Int]
		val allf = args.last.asInstanceOf[Boolean]
		
		if (allf && j.keySet != (t.columns.keySet - "id"))
			throw new CrasErrorException( "update: missing column(s) in PUT request" )
		else {
			val com = new StringBuilder( "update " )
			val last = t.names.last
			
			com ++= t.name
			com ++= " set "
			com ++=
				j.toList map {
					case (k, v) if t.columns(k).typ == StringType => k + "='" + String.valueOf( v ) + "'"
					case (k, v) => k + "=" + String.valueOf( v )
				} mkString ", "
			com ++= " where id="
			com ++= idv.toString
			env.statement.executeUpdate( com.toString )
		}
	}
}

object CommandNative extends Native( "command" ) {
	val argc = 1
	
	def apply( args: List[Any], env: Env ) = env.statement.executeUpdate( args.head.asInstanceOf[String] )
}

object OKNative extends Native( "OK" ) {
	val argc = 1
	
	def apply( args: List[Any], env: Env ) = Map( "status" -> "ok", "data" -> args.head )
}

object ErrorNative extends Native( "Error" ) {
	val argc = 1
	
	def apply( args: List[Any], env: Env ) = Map( "status" -> "error", "error" -> args.head )
}

object SingleOrNotFoundNative extends Native( "singleOrNotFound" ) {
	val argc = 1
	
	def apply( args: List[Any], env: Env ) = {
		val list = args.head.asInstanceOf[List[Any]]
		
		list.length match {
			case 0 => throw new CrasNotFoundException
			case 1 => list.head
			case _ => throw new CrasErrorException( "more than one item in list" )
		}
	}
}

object AtLeastOneOrNotFoundNative extends Native( "atLeastOneOrNotFound" ) {
	val argc = 1
	
	def apply( args: List[Any], env: Env ) = {
		val a = args.head.asInstanceOf[Int]
		
		a match {
			case 0 => throw new CrasNotFoundException
			case _ => a
//			case _ => throw new CrasErrorException( "more than one row updated" )
		}
	}
}

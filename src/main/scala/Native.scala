package xyz.hyperreal.cras

import java.lang.reflect.InvocationTargetException

import collection.mutable.{LinkedHashMap, HashMap}


object Native {
	def apply( f: Any ) = {
		val cla = f.getClass
		val methods = cla.getDeclaredMethods

		(for (m <- methods if !m.getName.startsWith("$")) yield {
			val classes = m.getParameterTypes map {
				p =>
					if (p.getName == "int")
						classOf[java.lang.Integer]
					else
						p} toList
				
			new Native2( m.getName, classes ) {
				def apply( args: List[Any], env: Env ) =
					try {
						m.invoke( f, (env +: args).asInstanceOf[List[AnyRef]]: _* )
					} catch {
						case e: InvocationTargetException => throw e.getCause
// 							if (e.getCause.isInstanceOf[CrasNotFoundException])
// 								throw new CrasNotFoundException
					}
			}
		}) toList
	}
	
	def apply( f: AnyRef, name: String, types: List[String] ) = {
		val classes =
			types map (t =>
				if (t contains '.')
					Class.forName( t )
				else
					Class.forName( "java.lang." + t ))
		val cla = f.getClass
		val method = {
			val methods = cla.getMethods filter (m => m.getName == "apply")
			
			if (methods.length == 0)
				sys.error( "no apply method" )
			else if (methods.length > 1)
				sys.error( "more than one apply method" )
				
			methods(0)
		}
		
		if (method.getParameterCount != types.length)
			sys.error( "wrong number of parameters" )
		
		new Native2( name, classes ) {
			def apply( args: List[Any], env: Env ) = method.invoke( f, (env +: args).asInstanceOf[List[AnyRef]]: _* )
		}
	}
}

abstract class Native2( val name: String, val classes: List[Class[_]] ) extends ((List[Any], Env) => Any) {
	val argc = classes.length
			
	require( classes.head == classOf[Env], "first parameter should be of type Env: " + name )

	def applicable( args: List[Any] ) =
		if (args.length == argc - 1) {
			args zip classes.drop(1) forall {case (arg, cla) => arg == null || cla.isInstance( arg )}
		} else
			false
			
	def apply( args: List[Any], env: Env ): Any
	
	override def toString = name + (classes map (c => c.getSimpleName) mkString ("(", ", ", ")"))
}

abstract class Native( val name: String ) extends ((List[Any], Env) => Any) {
	val argc: Int
	
	def apply( args: List[Any], env: Env ): Any
	
	override def toString = name + "/" + argc
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
					if (t.columns(c.toUpperCase).typ == StringType) {
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
					case (k, v) if t.columns(k.toUpperCase).typ == StringType => k + "='" + String.valueOf( v ) + "'"
					case (k, v) => k + "=" + String.valueOf( v )
				} mkString ", "
			com ++= " where id="
			com ++= idv.toString
			env.statement.executeUpdate( com.toString )
		}
	}
}

// object SingleOrNotFoundNative extends Native( "singleOrNotFound" ) {
// 	val argc = 1
// 	
// 	def apply( args: List[Any], env: Env ) = {
// 		val list = args.head.asInstanceOf[List[Any]]
// 		
// 		list.length match {
// 			case 0 => throw new CrasNotFoundException
// 			case 1 => list.head
// 			case _ => throw new CrasErrorException( "more than one item in list" )
// 		}
// 	}
// }

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

// object EvalNative extends Native2( "eval", List(classOf[Env], classOf[String]) ) {
// 	def apply( args: List[Any], env: Env ) = eval( args.head.asInstanceOf[String], env )
// }


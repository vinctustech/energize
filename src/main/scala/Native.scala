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
				
			new Native( m.getName, classes ) {
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
		
		new Native( name, classes ) {
			def apply( args: List[Any], env: Env ) = method.invoke( f, (env +: args).asInstanceOf[List[AnyRef]]: _* )
		}
	}
}

abstract class Native( val name: String, val classes: List[Class[_]] ) extends ((List[Any], Env) => Any) {
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

package xyz.hyperreal.energize

import collection.mutable.ArrayBuffer


object ResourceSorter extends TopologicalSorter[Resource] {
	def name( a: Resource ) = a.name

	def dependencies( a: Resource ) =
		a.fields filter (_.typ.isInstanceOf[ReferenceType]) map (_.typ.asInstanceOf[ReferenceType].resource)
}

abstract class TopologicalSorter[T] {
	def name( a: T ): String

	def dependencies( a: T ): Iterable[String]

	def sort( items: Iterable[T] ): Option[List[T]] = {
		val unsorted = new ArrayBuffer[T]
		val sorted = new ArrayBuffer[T]

		unsorted ++= items

		while (unsorted.nonEmpty) {
			val len = sorted.length

			for (e <- unsorted.toList)
				if (dependencies( e ) forall (d => sorted exists (name( _ ) == d))) {
					unsorted -= e
					sorted += e
				}
			
			if (len == sorted.length)
				return None
		}
		
		Some( sorted.toList )
	}
}

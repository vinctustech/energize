package xyz.hyperreal.energize

import collection.mutable.ArrayBuffer


object TableSorter extends TopologicalSorter[Table] {
	def name( a: Table ) = a.name

	def incoming(a: Table ) =
		a.columns.values filter (c => c.typ.isInstanceOf[ReferenceType]) map (c => c.typ.asInstanceOf[ReferenceType].table)
}

abstract class TopologicalSorter[T] {
	def name( a: T ): String

	def incoming(a: T ): Iterable[String]

	def sort( items: Iterable[T] ): Option[List[T]] = {
		val unsorted = new ArrayBuffer[T]
		val sorted = new ArrayBuffer[T]

		unsorted ++= items

		while (unsorted.nonEmpty) {
			val len = sorted.length

			for (e <- unsorted.toList)
				if (incoming( e ) forall (d => sorted exists (name( _ ) == d))) {
					unsorted -= e
					sorted += e
				}
			
			if (len == sorted.length)
				return None
		}
		
		Some( sorted.toList )
	}
}

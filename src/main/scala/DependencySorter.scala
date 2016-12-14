package xyz.hyperreal.cras

import collection.mutable.ArrayBuffer


// object TableSorter extends DependencySorter[Table] {
// 	def name( a: Table ) = a.name
// 	
// 	def dependencies( a: Table ) = a.columns filter (
// }

abstract class DependencySorter[T] {
	def name( a: T ): String
	
	def dependencies( a: T ): Seq[String]
	
	def sort( items: Seq[T] ): Option[List[T]] = {
		val unsorted = new ArrayBuffer[T]
		val sorted = new ArrayBuffer[T]
		
		unsorted ++= items

		while (!unsorted.isEmpty) {
			val len = sorted.length
		
			for (e <- unsorted)
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

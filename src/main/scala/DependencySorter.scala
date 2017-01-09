package xyz.hyperreal.cras

import collection.mutable.ArrayBuffer


object TableSorter extends DependencySorter[Table] {
	def name( a: Table ) = a.name
	
	def dependencies( a: Table ) =
		a.columns.values filter (c => c.typ.isInstanceOf[TableType]) map (c => c.typ.asInstanceOf[TableType].table)
}

abstract class DependencySorter[T] {
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

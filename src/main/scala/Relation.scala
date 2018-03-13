package xyz.hyperreal.energize

import java.sql._

import collection.mutable.ListBuffer


class Relation( db: Database, rs: ResultSet ) extends Iterable[IndexedSeq[AnyRef]] {
	private val md = rs.getMetaData
	private var cur: List[IndexedSeq[AnyRef]] = _

	val columnCount = md.getColumnCount
	val columns =
		for (i <- 1 to columnCount)
			yield
				Col( md.getTableName(i), nameOut(md.getColumnName(i)) )
	val columnMap = Map( (for (i <- 0 until columnCount) yield (columns(i), i)): _* )
	val rows = {
		val buf = new ListBuffer[IndexedSeq[AnyRef]]

		while (rs.next)
			buf += (for (i <- 1 to columnCount)
				yield
					rs.getObject( i ))

		buf.toList
	}

	def iterator = rows.iterator

	def next =
		if (cur eq null)
			if (rows == Nil)
				false
			else {
				cur = rows
				true
			}
		else if (cur.tail == Nil)
			false
		else {
			cur = cur.tail
			true
		}

	def get( index: Int ) = cur.head( index )

	def get( table: String, column: String ): Option[AnyRef] = {
		columnMap get Col(db.desensitize(table), db.desensitize(column)) map get
	}

	def getLong( table: String, column: String ) = get( table, column ).get.asInstanceOf[Long]

	override def toString =
		columns.toString + rows
}

case class Col( table: String, name: String )

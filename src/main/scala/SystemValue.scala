package xyz.hyperreal.energize


abstract class SystemValue {
	def value: Option[Any]
}

class SystemVariable extends SystemValue {
	var value: Option[Any] = None
}

class SystemConstant( const: Any ) extends SystemValue {
	private val _value = Some( const )

	def value = _value
}

object SystemValues extends Map[String, SystemValue] {

}
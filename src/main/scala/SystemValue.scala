package xyz.hyperreal.energize


abstract class SystemValue {
	def value: Any
}

class SystemVariable extends SystemValue {
	var value: Any = null
}

class SystemConstant( const: Any ) extends SystemValue {
	def value = const
}

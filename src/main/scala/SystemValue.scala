package xyz.hyperreal.energize


abstract class SystemValue {
	def value: AnyRef
}

class SystemVariable extends SystemValue {
	var value: AnyRef = null
}

class SystemConstant( const: AnyRef ) extends SystemValue {
	def value = const
}

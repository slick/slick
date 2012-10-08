package scala.slick.ast

/**
 * Super-trait for all types.
 */
trait Type

/**
 * Something that has a type.
 */
trait Typed {
  def tpe: Type
}

object Typed {
  def unapply(t: Typed) = Some(t.tpe)
}

sealed class StaticType(name: String) extends Type {
  override def toString = "StaticType."+name
}

object StaticType {
  object Boolean extends StaticType("Boolean")
  object Char extends StaticType("Char")
  object Int extends StaticType("Int")
  object Long extends StaticType("Long")
  object Null extends StaticType("Null")
  object String extends StaticType("String")
  object Unit extends StaticType("Unit")
}

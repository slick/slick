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

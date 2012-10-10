package scala.slick.ast

/** Super-trait for all types */
trait Type

/** Something that has a type */
trait Typed {
  def tpe: Type
}

object Typed {
  def unapply(t: Typed) = Some(t.tpe)
}

sealed class StaticType[T](name: String) extends Type {
  override def toString = "StaticType."+name
}

object StaticType {
  implicit object Boolean extends StaticType[Boolean]("Boolean")
  implicit object Char extends StaticType[Char]("Char")
  implicit object Int extends StaticType[Int]("Int")
  implicit object Long extends StaticType[Long]("Long")
  implicit object Null extends StaticType[Null]("Null")
  implicit object String extends StaticType[String]("String")
  implicit object Unit extends StaticType[Unit]("Unit")
}

trait StructType extends Type {
  def select(sym: Symbol): Type
}

trait OptionType extends Type {
  def elementType: Type
}

case class ProductType(elements: Seq[Type]) extends Type

case class CollectionType(cons: CollectionTypeConstructor, elementType: Type) extends Type

case class CollectionTypeConstructor(dummy: String = "")

object CollectionTypeConstructor {
  def default = new CollectionTypeConstructor
}

object NoType extends Type

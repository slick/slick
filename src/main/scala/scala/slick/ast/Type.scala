package scala.slick.ast

/** Super-trait for all types */
trait Type

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

/** Something that has a type */
trait Typed {
  def tpe: Type
}

object Typed {
  def unapply(t: Typed) = Some(t.tpe)
}

/* A Type that carries a Scala type argument */
trait TypedType[T] extends Type {
  def optionType: OptionTypedType[T] = new OptionTypedType[T](this)
}

trait BaseTypedType[T] extends TypedType[T]

class OptionTypedType[T](val elementType: TypedType[T]) extends TypedType[Option[T]] with OptionType

/** Mark a TypedType as eligible for numeric operators. */
trait NumericTypedType

object TypedType {
  @inline implicit def typedTypeToOptionTypedType[T](implicit t: TypedType[T]): OptionTypedType[T] = t.optionType
}

/** A basic type which must be provided by all drivers */
sealed class StaticType[T](name: String) extends BaseTypedType[T] {
  override def toString = "StaticType."+name
}

object StaticType {
  implicit object Boolean extends StaticType[Boolean]("Boolean")
  implicit object Char extends StaticType[Char]("Char")
  implicit object Int extends StaticType[Int]("Int") with NumericTypedType
  implicit object Long extends StaticType[Long]("Long") with NumericTypedType
  implicit object Null extends StaticType[Null]("Null")
  implicit object String extends StaticType[String]("String")
  implicit object Unit extends StaticType[Unit]("Unit")
}

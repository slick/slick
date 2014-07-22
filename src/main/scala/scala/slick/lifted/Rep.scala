package scala.slick.lifted

import scala.language.existentials
import scala.slick.ast._
import scala.slick.SlickException

/** Common base trait for all lifted values, including columns.
  *
  * All column operations are added with extension methods that depend on the type inside the `Rep`.
  * These are defined in:
  * <ul>
  *   <li>[[scala.slick.lifted.AnyExtensionMethods]] and [[scala.slick.lifted.ColumnExtensionMethods]] for columns of all types</li>
  *   <li>[[scala.slick.lifted.OptionColumnExtensionMethods]] for columns of all `Option` types</li>
  *   <li>[[scala.slick.lifted.PlainColumnExtensionMethods]] for columns of all non-`Option` types</li>
  *   <li>[[scala.slick.lifted.NumericColumnExtensionMethods]] for columns of numeric types (and Options thereof)</li>
  *   <li>[[scala.slick.lifted.BooleanColumnExtensionMethods]] for columns of `Boolean` / `Option[Boolean]`</li>
  *   <li>[[scala.slick.lifted.StringColumnExtensionMethods]] for columns of `String` / `Option[String]`</li>
  *   <li>[[scala.slick.lifted.ColumnOrdered]] for using columns in `sortBy` calls</li>
  * </ul>
  *
  * A `Rep[T : TypedType]` is always `Typed`, so that the `TypedType` can be retrieved directly
  * from the `Rep` value.
  */
trait Rep[T] {
  /** Encode a reference into this Rep. */
  def encodeRef(path: Node): Rep[T]

  /** Get the Node for this Rep. */
  def toNode: Node

  override def toString = s"Rep($toNode)"
}

object Rep {
  def forNode[T : TypedType](n: Node): Rep[T] = new TypedRep[T] { def toNode = n }
  def forNodeUntyped[T](n: Node): Rep[T] = new UntypedRep[T] { def toNode = n }

  abstract class TypedRep[T](implicit final val tpe: TypedType[T]) extends Rep[T] with Typed {
    def encodeRef(path: Node): Rep[T] = forNode(path)
  }

  abstract class UntypedRep[T] extends Rep[T] {
    def encodeRef(path: Node): Rep[T] = forNodeUntyped(path)
  }

  def columnPlaceholder[T : TypedType]: Rep[T] = new Rep[T] {
    def encodeRef(path: Node): Rep[T] = Rep.forNode[T](path)
    def toNode = throw new SlickException("Internal error: Cannot get Node from Rep.columnPlaceholder")
  }

  /** Lift a value inside a `Rep` into a `Some` Option value. */
  def Some[M, O](v: M)(implicit od: OptionLift[M, O]): O = od.lift(v)

  /** Create a `Rep` version of a `None` Option value. This is only supported for single-column values. */
  def None[T](implicit tpe: TypedType[T]): Rep[Option[T]] = LiteralColumn(scala.None)
}

/** A scalar value that is known at the client side at the time a query is executed.
  * This is either a constant value (`LiteralColumn`) or a scalar parameter. */
class ConstColumn[T : TypedType](val toNode: Node) extends Rep.TypedRep[T] {
  override def encodeRef(path: Node): ConstColumn[T] = new ConstColumn[T](path)
}

/** A column with a constant value which is inserted into an SQL statement as a literal. */
final case class LiteralColumn[T](value: T)(implicit tt: TypedType[T]) extends ConstColumn[T](LiteralNode(tt, value)) {
  /** Request that a bind variable be used instead of inserting a literal */
  def bind: Rep[T] = Rep.forNode[T](LiteralNode(tt, value, vol = true))
}

/** Represents `Rep[Option[T]]` in all cases where `T` is not a column base type. This special
  * representation is necessary so that a non-Option `Rep` value can be retrieved for encoding
  * Option-based operations. This base value is of type `T` if `T <: Rep[_]`, otherwise of
  * type `Rep[T]`. */
final case class RepOption[T](base: ShapedValue[_, _], toNode: Node) extends Rep[Option[T]] {
  def encodeRef(path: Node): Rep[Option[T]] = copy(toNode = path)
}

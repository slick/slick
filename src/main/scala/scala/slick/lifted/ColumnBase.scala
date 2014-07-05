package scala.slick.lifted

import scala.slick.ast._

/** Common base trait for all lifted values. */
trait Rep[T] {
  /** Encode a reference into this Rep */
  def encodeRef(path: List[Symbol]): Rep[T]

  /** Get the Node for this Rep */
  def toNode: Node
}

/** Base class for columns.
  *
  * Most operations are added with extension methods that depend on the type inside the `Column`.
  * These are defined in:
  * <ul>
  *   <li>[[scala.slick.lifted.AnyExtensionMethods]] and [[scala.slick.lifted.ColumnExtensionMethods]] for columns of all types</li>
  *   <li>[[scala.slick.lifted.OptionColumnExtensionMethods]] for columns of all `Option` types</li>
  *   <li>[[scala.slick.lifted.PlainColumnExtensionMethods]] for columns of all non-`Option` types</li>
  *   <li>[[scala.slick.lifted.NumericColumnExtensionMethods]] for columns of numeric types (and Options thereof)</li>
  *   <li>[[scala.slick.lifted.BooleanColumnExtensionMethods]] for columns of `Boolean` / `Option[Boolean]`</li>
  *   <li>[[scala.slick.lifted.StringColumnExtensionMethods]] for columns of `String` / `Option[String]`</li>
  * </ul>
  */
abstract class Column[T](implicit final val tpe: TypedType[T]) extends Rep[T] { self =>
  /** Order by this column in ascending order */
  def asc = ColumnOrdered[T](this, Ordering())
  /** Order by this column in descending order */
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))
  override def toString = s"Column($toNode)"
  def encodeRef(path: List[Symbol]): Column[T] = Column.forNode(Path(path))
}

object Column extends ColumnLowPriority {
  def forNode[T : TypedType](n: Node): Column[T] = new Column[T] { def toNode = n }

  /** A Shape for ConstColumns. It is identical to `columnShape` but it
    * ensures that a `ConstColumn[T]` packs to itself, not just to
    * `Column[T]`. This allows ConstColumns to be used as fully packed
    * types when compiling query functions. */
  @inline implicit def constColumnShape[T, Level <: ShapeLevel] = RepShape[Level, ConstColumn[T], T]
}

trait ColumnLowPriority {
  /** A Shape for Columns. */
  @inline implicit def columnShape[T, Level <: ShapeLevel] = RepShape[Level, Column[T], T]
}

/** A scalar value that is known at the client side at the time a query is executed. */
abstract class ConstColumn[T](implicit tt: TypedType[T]) extends Column[T] {
  override def encodeRef(path: List[Symbol]): ConstColumn[T] = new ConstColumn[T] { def toNode = Path(path) }
}

/** A scalar query parameter. This is a placeholder without a known value
  * which has to be compiled to a bind variable. */
class ParameterColumn[T](val toNode: Node)(implicit tt: TypedType[T]) extends ConstColumn[T]

/** A column with a constant value which is inserted into an SQL statement as a literal. */
final case class LiteralColumn[T](value: T)(implicit tt: TypedType[T]) extends ConstColumn[T] {
  /** Request that a bind variable be used instead of inserting a literal */
  def bind: Column[T] = Column.forNode[T](LiteralNode(tt, value, vol = true))
  def toNode = LiteralNode(tt, value)
}

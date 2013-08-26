package scala.slick.lifted

import scala.slick.ast._

/** Common base trait for all lifted values. */
trait Rep[T] {
  /** Encode a reference into this Rep */
  def encodeRef(path: List[Symbol]): Rep[T]

  /** Get the Node for this Rep */
  def toNode: Node
}

/** Common base trait for record values
  * (anything that is isomorphic to a tuple of scalar values). */
trait ColumnBase[T] extends Rep[T]

/** Base class for columns. */
abstract class Column[T](implicit final val tpe: TypedType[T]) extends ColumnBase[T] { self =>
  def asc = ColumnOrdered[T](this, Ordering())
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))
  override def toString = s"Column($toNode)"
  def encodeRef(path: List[Symbol]): Column[T] = Column.forNode(Path(path))
}

object Column {
  def forNode[T : TypedType](n: Node): Column[T] = new Column[T] { def toNode = n }
}

/** A column with a constant value which is inserted into an SQL statement as a literal. */
final case class ConstColumn[T](value: T)(implicit tt: TypedType[T]) extends Column[T] {
  def bind: Column[T] = Column.forNode[T](LiteralNode(tt, value, vol = true))
  def toNode = LiteralNode(tt, value)
}

package scala.slick.lifted

import scala.slick.ast._

/** Common base trait for all lifted values. */
trait Rep[T] extends NodeGenerator with WithOp

/** Common base trait for record values
  * (anything that is isomorphic to a tuple of scalar values). */
trait ColumnBase[T] extends Rep[T] with Typed

/** Base class for columns. */
abstract class Column[T : TypedType] extends ColumnBase[T] { self =>
  final val tpe = implicitly[TypedType[T]]
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)

  def asc = ColumnOrdered[T](this, Ordering())
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))

  override def toString = {
    val n = Node(this)
    if(n eq this) super.toString
    else s"Column($n)"
  }
}

object Column {
  def forNode[T : TypedType](n: Node): Column[T] = new Column[T] {
    def nodeDelegate = if(op eq null) n else op.nodeDelegate
  }
}

/** A column with a constant value which is inserted into an SQL statement as a literal. */
final case class ConstColumn[T](value: T)(implicit tt: TypedType[T]) extends Column[T] with LiteralNode {
  def bind: Column[T] = Column.forNode[T](LiteralNode(tt, value, vol = true))
  def nodeRebuild = copy()
  def volatileHint = false
}

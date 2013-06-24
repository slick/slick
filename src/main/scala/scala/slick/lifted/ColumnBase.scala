package scala.slick.lifted

import scala.slick.ast._

/** Common base trait for all lifted values. */
trait Rep[T] extends NodeGenerator

/** Common base trait for record values
  * (anything that is isomorphic to a tuple of scalar values). */
trait ColumnBase[T] extends Rep[T]

/** Base class for columns. */
abstract class Column[T](implicit final val tpe: TypedType[T]) extends ColumnBase[T] with EncodeRef { self =>
  def asc = ColumnOrdered[T](this, Ordering())
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))

  override def toString = {
    val n = Node(this)
    if(n eq this) super.toString
    else s"Column($n)"
  }
  def encodeRef(sym: Symbol, positions: List[Int] = Nil): Column[T] =
    Column.forNode(Path(positions.map(ElementSymbol) :+ sym))
}

object Column {
  def forNode[T : TypedType](n: Node): Column[T] = new Column[T] {
    def nodeDelegate = n
  }
}

/** A column with a constant value which is inserted into an SQL statement as a literal. */
final case class ConstColumn[T](value: T)(implicit tt: TypedType[T]) extends Column[T] with LiteralNode {
  def bind: Column[T] = Column.forNode[T](LiteralNode(tt, value, vol = true))
  def nodeRebuild = copy()
  def volatileHint = false
}

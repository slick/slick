package org.scalaquery.ql

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import org.scalaquery.{Shape, ShapedValue}
import org.scalaquery.ast._
import org.scalaquery.util.{ValueLinearizer, CollectionLinearizer}

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
abstract class Query[+E, U] extends Rep[AbstractCollection[Seq, U]] with CollectionLinearizer[Seq, U] { self =>

  def unpackable: ShapedValue[_ <: E, U]
  final lazy val packed = unpackable.packedNode
  final lazy val elementLinearizer = unpackable.linearizer
  final val canBuildFrom: CanBuildFrom[Nothing, U, Seq[U]] = implicitly

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val generator = new AnonSymbol
    val aliased = WithOp.mapOp(unpackable.value, (n => InRef(generator, n)))
    val fv = f(aliased)
    new WrappingQuery[F, T](new Bind(generator, Node(unpackable.value), Node(fv)), fv.unpackable)
  }

  def map[F, G, T](f: E => F)(implicit shape: Shape[F, T, G]): Query[G, T] =
    flatMap(v => Query[F, T, G](f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = InRef.forShapedValue(generator, unpackable)
    val fv = f(aliased.value)
    new WrappingQuery[E, U](Filter(generator, Node(this), Node(wt(fv))), unpackable)
  }

  def withFilter[T : CanBeQueryCondition](f: E => T) = filter(f)

  def where[T <: Column[_] : CanBeQueryCondition](f: E => T) = filter(f)

  def join[E2, U2](q2: Query[E2, U2], jt: JoinType = JoinType.Inner) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = InRef.forShapedValue(leftGen, unpackable)
    val aliased2 = InRef.forShapedValue(rightGen, q2.unpackable)
    new BaseJoinQuery[E, E2, U, U2](leftGen, rightGen, Node(unpackable.value), Node(q2.unpackable.value), jt, aliased1.zip(aliased2))
  }
  def innerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Inner)
  def leftJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Left)
  def rightJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Right)
  def outerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Outer)

  def sortBy[T <% Ordered](f: E => T): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = InRef.forShapedValue(generator, unpackable)
    new WrappingQuery[E, U](SortBy(generator, Node(this), f(aliased.value).columns), unpackable)
  }
  /*
  def groupBy(by: Column[_]*) =
    new Query[E, U](unpackable, cond, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList)
  */

  def union[O >: E, R](other: Query[O, U]) =
    new WrappingQuery[O, U](Union(Node(unpackable.value), Node(other.unpackable.value), false), unpackable)

  def unionAll[O >: E, R](other: Query[O, U]) =
    new WrappingQuery[O, U](Union(Node(unpackable.value), Node(other.unpackable.value), true), unpackable)

  def count = ColumnOps.CountAll(Node(unpackable.value))
  def exists = StdFunction[Boolean]("exists", Node(unpackable.value))

  @deprecated("Query.sub is not needed anymore", "0.10.0-M2")
  def sub = this

  def pack[R](implicit packing: Shape[E, _, R]): Query[R, U] =
    new Query[R, U] {
      val unpackable: ShapedValue[_ <: R, U] = self.unpackable.packedValue(packing)
      def nodeDelegate = self.nodeDelegate
    }

  // Query[Column[_]] only
  def asColumn(implicit ev: E <:< Column[_]): E =
    ev(unpackable.value).mapOp(_ => Node(this)).asInstanceOf[E]

  def take(num: Int): Query[E, U] = new WrappingQuery[E, U](Take(Node(this), num), unpackable)
  def drop(num: Int): Query[E, U] = new WrappingQuery[E, U](Drop(Node(this), num), unpackable)
}

object Query extends Query[Column[Unit], Unit] {
  def nodeDelegate = packed
  def unpackable = ShapedValue(ConstColumn(()).mapOp(Pure(_)), Shape.unpackColumnBase[Unit, Column[Unit]])
  def apply[E, U, R](value: E)(implicit unpack: Shape[E, U, R]) =
    apply[R, U](ShapedValue(value, unpack).packedValue)
  def apply[E, U](unpackable: ShapedValue[_ <: E, U]): Query[E, U] =
    if(unpackable.packedNode.isInstanceOf[AbstractTable[_]])
      new WrappingQuery[E, U](unpackable.packedNode, unpackable) {
        override lazy val unpackable = base.endoMap(n => WithOp.mapOp(n, { x => Node(this) }))
      }
    else new WrappingQuery[E, U](Pure(unpackable.packedNode), unpackable)

  @deprecated("Use .sortBy on a query instead of mixing in Query.orderBy", "0.10.0-M2")
  def orderBy[T <% Ordered](by: T) =
    new WrappingQuery[Column[Unit], Unit](OrderBy(new AnonSymbol, Node(this), by.columns), unpackable)
}

@implicitNotFound("Type ${T} cannot be a query condition (only Boolean, Column[Boolean] and Column[Option[Boolean]] are allowed")
trait CanBeQueryCondition[-T] extends (T => Column[_])

object CanBeQueryCondition {
  implicit object BooleanColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Boolean]] {
    def apply(value: Column[Boolean]) = value
  }
  implicit object BooleanOptionColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Option[Boolean]]] {
    def apply(value: Column[Option[Boolean]]) = value
  }
  implicit object BooleanCanBeQueryCondition extends CanBeQueryCondition[Boolean] {
    def apply(value: Boolean) = new ConstColumn(value)(TypeMapper.BooleanTypeMapper)
  }
}

class WrappingQuery[+E, U](val nodeDelegate: Node, val base: ShapedValue[_ <: E, U]) extends Query[E, U] {
  lazy val unpackable = Wrapped.wrapShapedValue(Node(this), base)
}

final class BaseJoinQuery[+E1, +E2, U1, U2](leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, base: ShapedValue[_ <: (E1, E2), (U1, U2)])
    extends WrappingQuery[(E1, E2), (U1,  U2)](BaseJoin(leftGen, rightGen, left, right, jt), base) {
  def on[T <: Column[_]](pred: (E1, E2) => T)(implicit wt: CanBeQueryCondition[T]) =
    new WrappingQuery[(E1, E2), (U1, U2)](FilteredJoin(leftGen, rightGen, left, right, jt, Node(wt(pred(base.value._1, base.value._2)))), base)
}

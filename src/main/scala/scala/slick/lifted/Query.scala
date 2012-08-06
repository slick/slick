package scala.slick.lifted

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.slick.ast.{Join => AJoin, _}
import scala.slick.util.CollectionLinearizer

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
abstract class Query[+E, U] extends Rep[Seq[U]] with CollectionLinearizer[Seq, U] { self =>

  def unpackable: ShapedValue[_ <: E, U]
  final lazy val packed = unpackable.packedNode
  final lazy val elementLinearizer = unpackable.linearizer
  final val canBuildFrom: CanBuildFrom[Nothing, U, Seq[U]] = implicitly

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val generator = new AnonSymbol
    val aliased = {
      val uv = unpackable.value
      WithOp.encodeRef(uv, generator)
    }
    val fv = f(aliased)
    new WrappingQuery[F, T](new Bind(generator, Node(unpackable.value), Node(fv)), fv.unpackable)
  }

  def map[F, G, T](f: E => F)(implicit shape: Shape[F, T, G]): Query[G, T] =
    flatMap(v => Query.pure[F, T, G](f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = unpackable.encodeRef(generator)
    val fv = f(aliased.value)
    new WrappingQuery[E, U](Filter(generator, Node(this), Node(wt(fv))), unpackable)
  }

  def withFilter[T : CanBeQueryCondition](f: E => T) = filter(f)

  def where[T <: Column[_] : CanBeQueryCondition](f: E => T) = filter(f)

  def join[E2, U2](q2: Query[E2, U2], jt: JoinType = JoinType.Inner) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = unpackable.encodeRef(leftGen)
    val aliased2 = q2.unpackable.encodeRef(rightGen)
    new BaseJoinQuery[E, E2, U, U2](leftGen, rightGen, Node(unpackable.value), Node(q2.unpackable.value), jt, aliased1.zip(aliased2))
  }
  def innerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Inner)
  def leftJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Left)
  def rightJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Right)
  def outerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Outer)
  def zip[E2, U2](q2: Query[E2, U2]): Query[(E, E2), (U, U2)] = join(q2, JoinType.Zip)
  def zipWith[E2, U2, F, G, T](q2: Query[E2, U2], f: (E, E2) => F)(implicit shape: Shape[F, T, G]): Query[G, T] =
    join(q2, JoinType.Zip).map[F, G, T](x => f(x._1, x._2))
  def zipWithIndex = zip(Query(RangeFrom(0L)))

  def sortBy[T <% Ordered](f: E => T): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = unpackable.encodeRef(generator)
    new WrappingQuery[E, U](SortBy(generator, Node(this), f(aliased.value).columns), unpackable)
  }

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[K, T, G], vshape: Shape[E, _, P]): Query[(G, Query[P, U]), (T, Query[P, U])] = {
    val sym = new AnonSymbol
    val key = ShapedValue(f(unpackable.encodeRef(sym).value), kshape).packedValue
    val value = ShapedValue(pack, Shape.selfLinearizingShape.asInstanceOf[Shape[Query[P, U], Query[P, U], Query[P, U]]])
    val group = GroupBy(sym, new AnonSymbol, Node(unpackable.value), Node(key.value))
    new WrappingQuery(group, key.zip(value))
  }

  def encodeRef(sym: Symbol, positions: List[Int] = Nil): Query[E, U] = new Query[E, U] {
    val unpackable = self.unpackable.encodeRef(sym, positions)
    lazy val nodeDelegate =
      positions.foldRight[Node](Ref(sym))((idx, node) => Select(node, ElementSymbol(idx)))
  }

  def union[O >: E, R](other: Query[O, U]) =
    new WrappingQuery[O, U](Union(Node(unpackable.value), Node(other.unpackable.value), false), unpackable)

  def unionAll[O >: E, R](other: Query[O, U]) =
    new WrappingQuery[O, U](Union(Node(unpackable.value), Node(other.unpackable.value), true), unpackable)

  def length: Column[Int] = Library.CountAll.column(Node(unpackable.value))
  @deprecated("Use .length instead of .count", "0.10.0-M2")
  def count = length
  def exists = Library.Exists.column[Boolean](Node(unpackable.value))

  @deprecated("Query.sub is not needed anymore", "0.10.0-M2")
  def sub = this

  def pack[R](implicit packing: Shape[E, _, R]): Query[R, U] =
    new Query[R, U] {
      val unpackable: ShapedValue[_ <: R, U] = self.unpackable.packedValue(packing)
      def nodeDelegate = self.nodeDelegate
    }

  def take(num: Int): Query[E, U] = new WrappingQuery[E, U](Take(Node(this), num), unpackable)
  def drop(num: Int): Query[E, U] = new WrappingQuery[E, U](Drop(Node(this), num), unpackable)
}

object Query extends Query[Column[Unit], Unit] {
  def nodeDelegate = packed
  def unpackable = ShapedValue(ConstColumn(()).mapOp((n, _) => Pure(n)), Shape.unpackColumnBase[Unit, Column[Unit]])

  @deprecated("Use .sortBy on a query instead of mixing in Query.orderBy", "0.10.0-M2")
  def orderBy[T <% Ordered](by: T) =
    new WrappingQuery[Column[Unit], Unit](OrderBy(new AnonSymbol, Node(this), by.columns), unpackable)

  def apply[E, U, R](value: E)(implicit unpack: Shape[E, U, R]): Query[R, U] = {
    val unpackable = ShapedValue(value, unpack).packedValue
    if(unpackable.packedNode.isInstanceOf[AbstractTable[_]])
      new NonWrappingQuery[R, U](unpackable.packedNode, unpackable)
    else new WrappingQuery[R, U](Pure(unpackable.packedNode), unpackable)
  }

  def pure[E, U, R](value: E)(implicit unpack: Shape[E, U, R]): Query[R, U] = {
    val unpackable = ShapedValue(value, unpack).packedValue
    new WrappingQuery[R, U](Pure(unpackable.packedNode), unpackable)
  }
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
    def apply(value: Boolean) = new ConstColumn(value)
  }
}

class WrappingQuery[+E, U](val nodeDelegate: Node, val base: ShapedValue[_ <: E, U]) extends Query[E, U] {
  lazy val unpackable = base.encodeRef(nodeDelegate.nodeIntrinsicSymbol)
}

class NonWrappingQuery[+E, U](val nodeDelegate: Node, val unpackable: ShapedValue[_ <: E, U]) extends Query[E, U]

final class BaseJoinQuery[+E1, +E2, U1, U2](leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, base: ShapedValue[_ <: (E1, E2), (U1, U2)])
    extends WrappingQuery[(E1, E2), (U1,  U2)](AJoin(leftGen, rightGen, left, right, jt, ConstColumn.TRUE), base) {
  def on[T <: Column[_]](pred: (E1, E2) => T)(implicit wt: CanBeQueryCondition[T]) =
    new WrappingQuery[(E1, E2), (U1, U2)](AJoin(leftGen, rightGen, left, right, jt, Node(wt(pred(base.value._1, base.value._2)))), base)
}

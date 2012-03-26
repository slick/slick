package org.scalaquery.ql

import org.scalaquery.ast._

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
abstract class Query[+E, +U] extends NodeGenerator {

  def unpackable: Unpackable[_ <: E, _ <: U]
  lazy val reified = unpackable.reifiedNode
  lazy val linearizer = unpackable.linearizer

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val generator = new AnonSymbol
    val aliased = WithOp.mapOp(unpackable.value, (n => InRef(generator, n)))
    val fv = f(aliased)
    new WrappingQuery[F, T](new Bind(generator, Node(unpackable.value), Node(fv)), fv.unpackable)
  }

  def map[F, G, T](f: E => F)(implicit unpack: Unpack[F, T], reify: Reify[F, G]): Query[G, T] =
    flatMap(v => Query[F, T, G](f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = {
    val generator = new AnonSymbol
    //val aliased = WithOp.mapOp(unpackable.value, (n => InRef(generator, n)))
    val aliased = InRef.forUnpackable(generator, unpackable)
    val fv = f(aliased.value)
    new WrappingQuery[E, U](Filter(generator, Node(this), Node(wt(fv))), unpackable)
  }

  def withFilter[T : CanBeQueryCondition](f: E => T) = filter(f)

  def where[T <: Column[_] : CanBeQueryCondition](f: E => T) = filter(f)

  def join[E2, U2](q2: Query[E2, U2], jt: JoinType = JoinType.Inner) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = InRef.forUnpackable(leftGen, unpackable)
    val aliased2 = InRef.forUnpackable(rightGen, q2.unpackable)
    new BaseJoinQuery[E, E2, U, U2](leftGen, rightGen, Node(unpackable.value), Node(q2.unpackable.value), jt, aliased1.zip(aliased2))
  }
  def innerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Inner)
  def leftJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Left)
  def rightJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Right)
  def outerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Outer)

  def sortBy[T <% Ordered](f: E => T): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = InRef.forUnpackable(generator, unpackable)
    new WrappingQuery[E, U](SortBy(generator, Node(this), f(aliased.value).columns), unpackable)
  }
  /*
  def groupBy(by: Column[_]*) =
    new Query[E, U](unpackable, cond, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList)
  */

  def cond: Seq[NodeGenerator] = Nil //--
  def modifiers: List[QueryModifier] = Nil //--
  def typedModifiers[T <: QueryModifier]: List[T] = Nil //--

  def union[O >: E, T >: U, R](other: Query[O, T]) = {
    new WrappingQuery[O, T](Union(Node(unpackable.value), Node(other.unpackable.value), false), unpackable)
  }

  def unionAll[O >: E, T >: U, R](other: Query[O, T]) = {
    new WrappingQuery[O, T](Union(Node(unpackable.value), Node(other.unpackable.value), true), unpackable)
  }

  def count = ColumnOps.CountAll(Node(unpackable.value))
  def exists = StdFunction[Boolean]("exists", Node(unpackable.value))

  @deprecated("Query.sub is not needed anymore", "0.10.0-M2")
  def sub = this

  //def reify[R](implicit reify: Reify[E, R]) =
  //  new Query[R, U](unpackable.reifiedUnpackable, cond, modifiers)

  // Query[Column[_]] only
  def asColumn(implicit ev: E <:< Column[_]): E =
    ev(unpackable.value).mapOp(_ => Node(this)).asInstanceOf[E]

  def take(num: Int): Query[E, U] = new WrappingQuery[E, U](Take(Node(this), num), unpackable)
  def drop(num: Int): Query[E, U] = new WrappingQuery[E, U](Drop(Node(this), num), unpackable)
}

object Query extends Query[Column[Unit], Unit] {
  def nodeDelegate = Pure(reified)
  def unpackable = Unpackable(ConstColumn(()), Unpack.unpackColumnBase[Unit])
  def apply[E, U, R](value: E)(implicit unpack: Unpack[E, U], reify: Reify[E, R]) =
    apply[R, U](Unpackable(value, unpack).reifiedUnpackable)
  def apply[E, U](unpackable: Unpackable[_ <: E, _ <: U]): Query[E, U] =
    if(unpackable.reifiedNode.isInstanceOf[AbstractTable[_]])
      new WrappingQuery[E, U](unpackable.reifiedNode, unpackable) {
        override lazy val unpackable = base.endoMap(n => WithOp.mapOp(n, { x => Node(this) }))
      }
    else new WrappingQuery[E, U](Pure(unpackable.reifiedNode), unpackable)

  @deprecated("Use .sortBy on a query instead of mixing in Query.orderBy", "0.10.0-M2")
  def orderBy[T <% Ordered](by: T) =
    new WrappingQuery[Column[Unit], Unit](SortBy(new AnonSymbol, Node(this), by.columns), unpackable)
}

trait CanBeQueryCondition[-T] {
  def apply(value: T): Column[_]
}

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

//TODO remove
final case class Subquery(query: Node, rename: Boolean) extends UnaryNode {
  val child = Node(query)
  protected[this] override def nodeChildNames = Seq("query")
  protected[this] def nodeRebuild(child: Node): Node = copy(query = child)
  override def isNamedTable = true
}

//TODO remove
final case class SubqueryColumn(pos: Int, subquery: Node, typeMapper: TypeMapper[_]) extends UnaryNode {
  val child = Node(subquery)
  protected[this] override def nodeChildNames = Seq("subquery")
  protected[this] def nodeRebuild(child: Node): Node = copy(subquery = child)
  override def toString = "SubqueryColumn c"+pos
}

class WrappingQuery[+E, +U](val nodeDelegate: Node, val base: Unpackable[_ <: E, _ <: U]) extends Query[E, U] {
  lazy val unpackable = Wrapped.wrapUnpackable(Node(this), base)
}

final class BaseJoinQuery[+E1, +E2, +U1, +U2](leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, base: Unpackable[_ <: (E1, E2), _ <: (U1, U2)])
    extends WrappingQuery[(E1, E2), (U1,  U2)](BaseJoin(leftGen, rightGen, left, right, jt), base) {
  def on[T <: Column[_]](pred: (E1, E2) => T)(implicit wt: CanBeQueryCondition[T]) =
    new WrappingQuery[(E1, E2), (U1, U2)](FilteredJoin(leftGen, rightGen, left, right, jt, Node(wt(pred(base.value._1, base.value._2)))), base)
}

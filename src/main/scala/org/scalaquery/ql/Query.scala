package org.scalaquery.ql

import org.scalaquery.ast._
import org.scalaquery.ql.Join.JoinType

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
abstract class Query[+E, +U]() extends NodeGenerator {

  def unpackable: Unpackable[_ <: E, _ <: U]
  lazy val reified = unpackable.reifiedNode
  lazy val linearizer = unpackable.linearizer

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val generator = new AnonSymbol
    val aliased = WithOp.mapOp(unpackable.value, (n => InRef(generator, n)))
    val fv = f(aliased)
    new WrappingQuery[F, T](new Bind(generator, Node(unpackable.value), Node(fv)), fv.unpackable)
  }

  def map[F, T](f: E => F)(implicit unpack: Unpack[F, T]): Query[F, T] = flatMap(v => Query(f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T, R](f: E => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[E, R]): Query[R, U] = {
    val generator = new AnonSymbol
    //val aliased = WithOp.mapOp(unpackable.value, (n => InRef(generator, n)))
    val aliased = InRef.forUnpackable(generator, unpackable)
    val fv = f(aliased.value)
    new WrappingQuery[R, U](Filter(generator, Node(this), Node(wt(fv))), unpackable.reifiedUnpackable(reify))
  }

  def withFilter[T, R](f: E => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[E, R]) = filter(f)(wt, reify)

  def where[T <: Column[_], R](f: E => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[E, R]) = filter(f)(wt, reify)

  def join[E2, U2, R1, R2](q2: Query[E2, U2])(implicit reify1: Reify[E, R1], reify2: Reify[E2, R2]) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = InRef.forUnpackable(leftGen, unpackable)
    val aliased2 = InRef.forUnpackable(rightGen, q2.unpackable)
    new BaseJoinQuery[R1, R2, U, U2](leftGen, rightGen, Node(unpackable.value), Node(q2.unpackable.value), Join.Inner, aliased1.zip(aliased2).reifiedUnpackable)
  }

  /*
  def groupBy(by: Column[_]*) =
    new Query[E, U](unpackable, cond, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList)

  def orderBy(by: Ordering*) = new Query[E, U](unpackable, cond, modifiers ::: by.toList)

  def exists = StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))
  */

  def cond: Seq[NodeGenerator] = Nil //--
  def modifiers: List[QueryModifier] = Nil //--
  def typedModifiers[T <: QueryModifier]: List[T] = Nil //--

  /*
  // Unpackable queries only
  def union[O >: E, T >: U, R](other: Query[O, T]*)(implicit reify: Reify[O, R]) = wrap(Union(false, this :: other.toList))

  def unionAll[O >: E, T >: U, R](other: Query[O, T]*)(implicit reify: Reify[O, R]) = wrap(Union(true, this :: other.toList))

  def count = ColumnOps.CountAll(Subquery(this, false))

  def sub[UU >: U, R](implicit reify: Reify[E, R]) = wrap(this)

  //def reify[R](implicit reify: Reify[E, R]) =
  //  new Query[R, U](unpackable.reifiedUnpackable, cond, modifiers)

  // Query[Column[_]] only
  def asColumn(implicit ev: E <:< Column[_]): E = unpackable.value.asInstanceOf[WithOp].mapOp(_ => this).asInstanceOf[E]
  */
}

object Query extends Query[Unit, Unit] {
  def nodeDelegate = reified
  def unpackable = Unpackable((), Unpack.unpackPrimitive[Unit])
  def apply[E, U](value: E)(implicit unpack: Unpack[E, U]) = apply[E, U](Unpackable(value, unpack))
  def apply[E, U](unpackable: Unpackable[_ <: E, _ <: U]): Query[E, U] =
    if(unpackable.reifiedNode.isInstanceOf[AbstractTable[_]])
      new WrappingQuery[E, U](new TableQuery(unpackable.reifiedNode), unpackable) {
        override lazy val unpackable = base.endoMap(n => WithOp.mapOp(n, { x => Node(this) }))
      }
    else new WrappingQuery[E, U](Pure(unpackable.reifiedNode), unpackable)
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

//TODO remove
final case class Union(all: Boolean, queries: IndexedSeq[Node]) extends SimpleNode {
  protected[this] def nodeChildGenerators = queries
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = copy(queries = ch)
  override def toString = if(all) "Union all" else "Union"
}

class WrappingQuery[+E, +U](val nodeDelegate: Node, val base: Unpackable[_ <: E, _ <: U]) extends Query[E, U] {
  lazy val unpackable = Wrapped.wrapUnpackable(Node(this), base)
}

final class BaseJoinQuery[+E1, +E2, +U1, +U2](leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, base: Unpackable[_ <: (E1, E2), _ <: (U1, U2)])
    extends WrappingQuery[(E1, E2), (U1,  U2)](BaseJoin(leftGen, rightGen, left, right, jt), base) {
  def on[T <: Column[_], R](pred: (E1, E2) => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[(E1, E2), R]) =
    new WrappingQuery[(E1, E2), (U1, U2)](FilteredJoin(leftGen, rightGen, left, right, jt, Node(wt(pred(base.value._1, base.value._2)))), base)
}

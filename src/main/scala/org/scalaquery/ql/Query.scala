package org.scalaquery.ql

import scala.reflect.Manifest
import org.scalaquery.SQueryException
import org.scalaquery.util.{Node, WithOp}

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[+E, +U](val unpackable: Unpackable[_ <: E, _ <: U], val cond: List[Column[_]],
                val modifiers: List[QueryModifier]) extends Node {

  lazy val reified = unpackable.reifiedNode
  lazy val linearizer = unpackable.linearizer

  def nodeChildren = reified :: cond

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = new Bind[F, T](f(unpackable.value), this)

  def map[F, T](f: E => F)(implicit unpack: Unpack[F, T]): Query[F, T] = flatMap(v => Query(f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T, R](f: E => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[E, R]): Query[R, U] =
    new Filter[R, U](this, unpackable.reifiedUnpackable(reify), wt(f(unpackable.value)))

  def withFilter[T, R](f: E => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[E, R]) = filter(f)(wt, reify)

  def where[T <: Column[_], R](f: E => T)(implicit wt: CanBeQueryCondition[T], reify: Reify[E, R]) = filter(f)(wt, reify)

  /*
  def groupBy(by: Column[_]*) =
    new Query[E, U](unpackable, cond, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList)

  def orderBy(by: Ordering*) = new Query[E, U](unpackable, cond, modifiers ::: by.toList)

  def exists = StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))
  */

  def typedModifiers[T <: QueryModifier](implicit m: ClassManifest[T]) =
    modifiers.filter(m.erasure.isInstance(_)).asInstanceOf[List[T]]

  def createOrReplaceSingularModifier[T <: QueryModifier](f: Option[T] => T)(implicit m: Manifest[T]): Query[E, U] = {
    val (xs, other) = modifiers.partition(m.erasure.isInstance(_))
    val mod = xs match {
      case x :: _ => f(Some(x.asInstanceOf[T]))
      case _ => f(None)
    }
    new Query[E, U](unpackable, cond, mod :: other)
  }

  /*
  // Unpackable queries only
  def union[O >: E, T >: U, R](other: Query[O, T]*)(implicit reify: Reify[O, R]) = wrap(Union(false, this :: other.toList))

  def unionAll[O >: E, T >: U, R](other: Query[O, T]*)(implicit reify: Reify[O, R]) = wrap(Union(true, this :: other.toList))

  def count = ColumnOps.CountAll(Subquery(this, false))

  def sub[UU >: U, R](implicit reify: Reify[E, R]) = wrap(this)

  private def wrap[R](base: Node)(implicit reify: Reify[E, R]): Query[R, U] = {
    def f[EE](unpackable: Unpackable[EE, _ <: U]) = unpackable.endoMap(v => v match {
      case t:AbstractTable[_] =>
        t.mapOp(_ => Subquery(base, false)).asInstanceOf[EE]
      case o =>
        var pos = 0
        val p = Subquery(base, true)
        unpackable.mapOp { v =>
          pos += 1
          SubqueryColumn(pos, p, v match {
            case c: Column[_] => c.typeMapper
            case SubqueryColumn(_, _, tm) => tm
            case _ => throw new SQueryException("Expected Column or SubqueryColumn")
          })
        }
    })
    val r: Unpackable[R, _ <: U] = unpackable.reifiedUnpackable(reify)
    Query[R, U](f(r))
  }

  //def reify[R](implicit reify: Reify[E, R]) =
  //  new Query[R, U](unpackable.reifiedUnpackable, cond, modifiers)

  // Query[Column[_]] only
  def asColumn(implicit ev: E <:< Column[_]): E = unpackable.value.asInstanceOf[WithOp].mapOp(_ => this).asInstanceOf[E]
  */
}

object Query extends Pure[Unit, Unit](Unpackable((), Unpack.unpackPrimitive[Unit])) {
  def apply[E, U](value: E)(implicit unpack: Unpack[E, U]) = new Pure[E, U](Unpackable(value, unpack))
  def apply[E, U](unpackable: Unpackable[_ <: E, _ <: U]) = new Pure[E, U](unpackable)
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

case class Subquery(query: Node, rename: Boolean) extends Node {
  def nodeChildren = query :: Nil
  override def nodeNamedChildren = (query, "query") :: Nil
  override def isNamedTable = true
}

case class SubqueryColumn(pos: Int, subquery: Subquery, typeMapper: TypeMapper[_]) extends Node {
  def nodeChildren = subquery :: Nil
  override def nodeNamedChildren = (subquery, "subquery") :: Nil
  override def toString = "SubqueryColumn c"+pos
}

case class Union(all: Boolean, queries: List[Query[_, _]]) extends Node {
  override def toString = if(all) "Union all" else "Union"
  def nodeChildren = queries
}

class Pure[+E, +U](_unpackable: Unpackable[_ <: E, _ <: U]) extends Query[E, U](_unpackable, Nil, Nil) {
  override def toString = "Pure"
  override def nodeChildren = List(reified)
  override def nodeNamedChildren = (reified, "value") :: Nil
}

class Filter[+E, +U](val from: Query[_,_], base: Unpackable[_ <: E, _ <: U], _cond: Column[_]) extends Query[E, U](null, List(_cond), Nil) {
  override val unpackable = base.endoMap(n => WithOp.mapOp(n, { x => Wrapped(Node(x), Node(this)) }))
  override def toString = "Filter"
  override def nodeChildren = from :: cond
  override def nodeNamedChildren = (from, "from") :: cond.map(n => (Node(n), "where"))
  override def isNamedTable = true
}

class Bind[+E, +U](sub: Query[E, U], val from: Query[_,_]) extends Query[E, U](sub.unpackable, Nil, Nil) {
  override def toString = "Bind"
  override def nodeChildren = sub :: from :: cond.map(Node.apply)
  override def nodeNamedChildren = (from, "from") :: (sub, "select") :: cond.map(n => (Node(n), "where"))
}

case class Wrapped(what: Node, in: Node) extends Node {
  def nodeChildren = what :: in :: Nil
  override def nodeNamedChildren = (what, "what") :: (in, "in") :: Nil
}

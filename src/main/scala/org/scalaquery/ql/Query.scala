package org.scalaquery.ql

import scala.reflect.Manifest
import org.scalaquery.SQueryException
import org.scalaquery.util.{Node, WithOp}

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[+E, +U](val unpackable: Unpackable[_ <: E, _ <: U], val cond: List[Column[_]],  val condHaving: List[Column[_]],
                val modifiers: List[QueryModifier]) extends Node {

  lazy val reified = unpackable.reifiedNode
  lazy val linearizer = unpackable.linearizer

  def nodeChildren = reified :: cond.map(Node.apply) ::: modifiers
  override def nodeNamedChildren = (reified, "select") :: cond.map(n => (Node(n), "where")) ::: modifiers.map(o => (o, "modifier"))

  override def toString = "Query"

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val q = f(unpackable.value)
    new Query[F, T](q.unpackable, cond ::: q.cond, condHaving ::: q.condHaving, modifiers ::: q.modifiers)
  }

  def map[F, T](f: E => F)(implicit unpack: Unpack[F, T]): Query[F, T] = flatMap(v => Query(f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] =
    new Query[E, U](unpackable, wt(f(unpackable.value), cond), condHaving, modifiers)

  def withFilter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = filter(f)(wt)

  def where[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = filter(f)(wt)

  def having[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] =
    new Query[E, U](unpackable, cond, wt(f(unpackable.value), condHaving), modifiers)

  def groupBy(by: Column[_]*) =
    new Query[E, U](unpackable, cond, condHaving, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList)

  def orderBy(by: Ordering*) = new Query[E, U](unpackable, cond, condHaving, modifiers ::: by.toList)

  def exists = StdFunction[Boolean]("exists", map(_ => ConstColumn(1)))

  def typedModifiers[T <: QueryModifier](implicit m: ClassManifest[T]) =
    modifiers.filter(m.erasure.isInstance(_)).asInstanceOf[List[T]]

  def createOrReplaceSingularModifier[T <: QueryModifier](f: Option[T] => T)(implicit m: Manifest[T]): Query[E, U] = {
    val (xs, other) = modifiers.partition(m.erasure.isInstance(_))
    val mod = xs match {
      case x :: _ => f(Some(x.asInstanceOf[T]))
      case _ => f(None)
    }
    new Query[E, U](unpackable, cond, condHaving, mod :: other)
  }

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
  //  new Query[R, U](unpackable.reifiedUnpackable, cond, condHaving, modifiers)

  // Query[Column[_]] only
  def asColumn(implicit ev: E <:< Column[_]): E = unpackable.value.asInstanceOf[WithOp].mapOp(_ => this).asInstanceOf[E]
}

object Query extends Query[Unit, Unit](Unpackable((), Unpack.unpackPrimitive[Unit]), Nil, Nil, Nil) {
  def apply[E, U](value: E)(implicit unpack: Unpack[E, U]) = new Query[E, U](Unpackable(value, unpack), Nil, Nil, Nil)
  def apply[E, U](unpackable: Unpackable[_ <: E, _ <: U]) = new Query[E, U](unpackable, Nil, Nil, Nil)
}

trait CanBeQueryCondition[-T] {
  def apply(value: T, l: List[Column[_]]): List[Column[_]]
}

object CanBeQueryCondition {
  implicit object BooleanColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Boolean]] {
    def apply(value: Column[Boolean], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object BooleanOptionColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Option[Boolean]]] {
    def apply(value: Column[Option[Boolean]], l: List[Column[_]]): List[Column[_]] = value :: l
  }
  implicit object BooleanCanBeQueryCondition extends CanBeQueryCondition[Boolean] {
    def apply(value: Boolean, l: List[Column[_]]): List[Column[_]] =
      if(value) l else new ConstColumn(false)(TypeMapper.BooleanTypeMapper) :: Nil
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

package org.scalaquery.ql

import scala.reflect.Manifest
import org.scalaquery.SQueryException
import org.scalaquery.util.{Node, WithOp}
import =>>.CanUnpack

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[E, +U](val value: E, val cond: List[Column[_]],  val condHaving: List[Column[_]],
                val modifiers: List[QueryModifier], val unpack: E =>> U) extends Node {

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: modifiers
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) ::: modifiers.map(o => (o, "modifier"))

  override def toString = "Query"

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val q = f(value)
    new Query[F, T](q.value, cond ::: q.cond, condHaving ::: q.condHaving, modifiers ::: q.modifiers, q.unpack)
  }

  def map[F, T](f: E => F)(implicit unpack: F =>> T): Query[F, T] = flatMap(v => Query(f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] =
    new Query[E, U](value, wt(f(value), cond), condHaving, modifiers, unpack)

  def withFilter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = filter(f)(wt)

  def where[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = filter(f)(wt)

  def having[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] =
    new Query[E, U](value, cond, wt(f(value), condHaving), modifiers, unpack)

  def groupBy(by: Column[_]*) =
    new Query[E, U](value, cond, condHaving, modifiers ::: by.view.map(c => new Grouping(Node(c))).toList, unpack)

  def orderBy(by: Ordering*) = new Query[E, U](value, cond, condHaving, modifiers ::: by.toList, unpack)

  def exists = ColumnOps.Exists(map(_ => ConstColumn(1)))

  def typedModifiers[T <: QueryModifier](implicit m: ClassManifest[T]) =
    modifiers.filter(m.erasure.isInstance(_)).asInstanceOf[List[T]]

  def createOrReplaceSingularModifier[T <: QueryModifier](f: Option[T] => T)(implicit m: Manifest[T]): Query[E, U] = {
    val (xs, other) = modifiers.partition(m.erasure.isInstance(_))
    val mod = xs match {
      case x :: _ => f(Some(x.asInstanceOf[T]))
      case _ => f(None)
    }
    new Query[E, U](value, cond, condHaving, mod :: other, unpack)
  }

  // Unpackable queries only
  def union[O >: E, T >: U](other: Query[O, T]*) = wrap(Union(false, this :: other.toList))

  def unionAll[O >: E, T >: U](other: Query[O, T]*) = wrap(Union(true, this :: other.toList))

  def count = ColumnOps.CountAll(Subquery(this, false))

  def sub = wrap(this)

  private[this] def wrap(base: Node): Query[E, U] = Query[E, U](value match {
    case t:AbstractTable[_] =>
      t.mapOp(_ => Subquery(base, false)).asInstanceOf[E]
    case o =>
      var pos = 0
      val p = Subquery(base, true)
      WithOp.mapOp(o, { v =>
        pos += 1
        SubqueryColumn(pos, p, v match {
          case c: Column[_] => c.typeMapper
          case SubqueryColumn(_, _, tm) => tm
          case _ => throw new SQueryException("Expected Column or SubqueryColumn")
        })
      })
  })(unpack)

  // Query[Column[_]] only
  def asColumn(implicit ev: E <:< Column[_]): E = value.asInstanceOf[WithOp].mapOp(_ => this).asInstanceOf[E]
}

object Query extends Query[Unit, Unit]((), Nil, Nil, Nil, =>>.unpackUnit) {
  def apply[E, U](value: E)(implicit unpack: E =>> U) = new Query[E, U](value, Nil, Nil, Nil, unpack)
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

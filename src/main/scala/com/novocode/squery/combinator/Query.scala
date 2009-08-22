package com.novocode.squery.combinator

import scala.reflect.Manifest

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[+E](val value: E, val cond: List[Column[_]],  val condHaving: List[Column[_]],
                val modifiers: List[QueryModifier]) extends Node {

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: modifiers
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) ::: modifiers.map(o => (o, "modifier"))

  override def toString = "Query"

  def flatMap[F](f: E => Query[F]): Query[F] = {
    val q = f(value)
    new Query(q.value, cond ::: q.cond, condHaving ::: q.condHaving, modifiers ::: q.modifiers)
  }

  def map[F](f: E => F): Query[F] = flatMap(v => Query(f(v)))

  def >>[F](q: Query[F]): Query[F] = flatMap(_ => q)

  def where[T <: Column[_]](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, wt(f(value), cond), condHaving, modifiers)

  def filter[T](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, wt(f(value), cond), condHaving, modifiers)

  def having[T <: Column[_]](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, cond, wt(f(value), condHaving), modifiers)

  def groupBy(by: Column[_]*) =
    new Query[E](value, cond, condHaving, modifiers ::: by.projection.map(c => new Grouping(Node(c))).toList)

  def orderBy(by: Ordering*) = new Query[E](value, cond, condHaving, modifiers ::: by.toList)

  def exists = Operator.Exists(map(_ => ConstColumn(1)))

  def typedModifiers[T <: QueryModifier](implicit m: Manifest[T]) =
    modifiers.filter(m.erasure.isInstance(_)).asInstanceOf[List[T]]

  def createOrReplaceSingularModifier[T <: QueryModifier](f: Option[T] => T)(implicit m: Manifest[T]): Query[E] = {
    val (xs, other) = modifiers.partition(m.erasure.isInstance(_))
    val mod = xs match {
      case x :: _ => f(Some(x.asInstanceOf[T]))
      case _ => f(None)
    }
    new Query[E](value, cond, condHaving, mod :: other)
  }
}

object Query extends Query[Unit]((), Nil, Nil, Nil) {
  def apply[E](value: E) = new Query(value, Nil, Nil, Nil)

  trait WhereType[-T] {
    def apply(value: T, l: List[Column[_]]): List[Column[_]]
  }
}

class QueryOfColumnBaseOps[E <: ColumnBase[_]](q: Query[E]) {
  
  def union(other: Query[E]*) = wrap(Union(false, q :: other.toList))

  def unionAll(other: Query[E]*) = wrap(Union(true, q :: other.toList))

  def count = Operator.CountAll(Subquery(q, false))

  def sub = wrap(q)

  private[this] def wrap(base: Node): Query[E] = Query(q.value match {
    case t:Table[_] =>
      q.value.mapOp(_ => Subquery(base, false))
    case _ =>
      var pos = 0
      val p = Subquery(base, true)
      q.value.mapOp { v =>
        pos += 1
        SubqueryColumn(pos, p)
      }
  })
}

class QueryOfColumnOps[E <: Column[_]](q: Query[E]) {
  def asColumn: E = q.value.mapOp(_ => q)
}

case class Subquery(query: Node, rename: Boolean) extends Node {
  def nodeChildren = query :: Nil
  override def nodeNamedChildren = (query, "query") :: Nil
  override def isNamedTable = true
}

case class SubqueryColumn(pos: Int, subquery: Subquery) extends Node {
  def nodeChildren = subquery :: Nil
  override def nodeNamedChildren = (subquery, "subquery") :: Nil
  override def toString = "SubqueryColumn c"+pos
}

case class Union(all: Boolean, queries: List[Query[_]]) extends Node {
  override def toString = if(all) "Union all" else "Union"
  def nodeChildren = queries
}

package com.novocode.squery.combinator

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[+E](val value: E, val cond: List[Column[_]],  val condHaving: List[Column[_]],
                val groupings: List[Grouping], val orderings: List[Ordering]) extends Node {

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: groupings ::: orderings
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) :::
    groupings.map(o => (o, "groupings")) ::: orderings.map(o => (o, "orderings"))

  override def toString = "Query"

  def flatMap[F](f: E => Query[F]): Query[F] = {
    val q = f(value)
    new Query(q.value, cond ::: q.cond, condHaving ::: q.condHaving, groupings ::: q.groupings, orderings ::: q.orderings)
  }

  def map[F](f: E => F): Query[F] = flatMap(v => Query(f(v)))

  def >>[F](q: Query[F]): Query[F] = flatMap(_ => q)

  def where[T <: Column[_]](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, wt(f(value), cond), condHaving, groupings, orderings)

  def filter[T](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, wt(f(value), cond), condHaving, groupings, orderings)

  def having[T <: Column[_]](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, cond, wt(f(value), condHaving), groupings, orderings)

  def groupBy(by: Column[_]*) =
    new Query[E](value, cond, condHaving, groupings ::: by.projection.map(c => new Grouping(Node(c))).toList, orderings)

  def orderBy(by: Ordering*) = new Query[E](value, cond, condHaving, groupings, orderings ::: by.toList)

  def exists = Operator.Exists(map(_ => ConstColumn(1)))
}

object Query extends Query[Unit]((), Nil, Nil, Nil, Nil) {
  def apply[E](value: E) = new Query(value, Nil, Nil, Nil, Nil)

  trait WhereType[-T] {
    def apply(value: T, l: List[Column[_]]): List[Column[_]]
  }
}

class QueryOfColumnBaseOps[E <: ColumnBase[_]](q: Query[E]) {
  
  def union(other: Query[E]*) = wrap(Union(false, q :: other.toList))

  def unionAll(other: Query[E]*) = wrap(Union(true, q :: other.toList))

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

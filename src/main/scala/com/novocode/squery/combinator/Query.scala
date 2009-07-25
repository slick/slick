package com.novocode.squery.combinator

import java.io.PrintWriter

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[+E](val value: E, val cond: List[Column[_]],  val condHaving: List[Column[_]],
                val groupBy: List[GroupBy.Grouping], val orderBy: List[OrderBy.Ordering]) extends Node {

  def flatMap[F](f: E => Query[F]): Query[F] = {
    val q = f(value)
    new Query(q.value, cond ::: q.cond, condHaving ::: q.condHaving, groupBy ::: q.groupBy, orderBy ::: q.orderBy)
  }

  def map[F](f: E => F): Query[F] = flatMap(v => Query(f(v)))

  def >>[F](q: Query[F]): Query[F] = flatMap(_ => q)

  def where[T <: Column[_]](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, wt(f(value), cond), condHaving, groupBy, orderBy)

  def filter[T](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, wt(f(value), cond), condHaving, groupBy, orderBy)

  def having[T <: Column[_]](f: E => T)(implicit wt: Query.WhereType[T]): Query[E] =
    new Query(value, cond, wt(f(value), condHaving), groupBy, orderBy)

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: groupBy ::: orderBy
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) :::
    groupBy.map(o => (o, "groupBy")) ::: orderBy.map(o => (o, "orderBy"))

  override def toString = "Query"

  // Convenience methods
  def orderBy(f: E => Column[_]): Query[E] = for {
    x <- this
    _ <- OrderBy +f(value)
  } yield x
}

object Query {
  def apply[E](value: E) = new Query(value, Nil, Nil, Nil, Nil)

  trait WhereType[-T] {
    def apply(value: T, l: List[Column[_]]): List[Column[_]]
  }
}

class QueryOfColumnOps[E <: ColumnBase[_]](q: Query[E]) {
  
  def union(other: Query[E]): Query[E] = {
    val u = Union(false, q, other)
    Query(q.value.mapOp(n => Union.UnionPart(n, u)))
  }
}

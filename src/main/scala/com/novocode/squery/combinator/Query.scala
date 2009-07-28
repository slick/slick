package com.novocode.squery.combinator

import java.io.PrintWriter

/**
 * A query monad which contains the AST for a query's projection and the accumulated
 * restrictions and other modifiers.
 */
class Query[+E](val value: E, val cond: List[Column[_]],  val condHaving: List[Column[_]],
                val groupings: List[Grouping], val orderings: List[Ordering]) extends Node {

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

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: groupings ::: orderings
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) :::
    groupings.map(o => (o, "groupings")) ::: orderings.map(o => (o, "orderings"))

  override def toString = "Query"

  def groupBy(by: Column[_]*) =
    new Query[E](value, cond, condHaving, groupings ::: by.projection.map(c => new Grouping(Node(c))).toList, orderings)
  def orderBy(by: Ordering*) = new Query[E](value, cond, condHaving, groupings, orderings ::: by.toList)
}

object Query extends Query[Unit]((), Nil, Nil, Nil, Nil) {
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

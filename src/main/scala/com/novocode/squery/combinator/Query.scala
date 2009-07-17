package com.novocode.squery.combinator

import java.io.PrintWriter


class Query[+E <: AnyRef](val value: E, val cond: List[Column[_]], val ordering: List[Order.Ordering]) extends Node {

  def select[F <: AnyRef](f: E => Query[F]): Query[F] = {
    val q = f(value)
    new Query(q.value, cond ::: q.cond, ordering ::: q.ordering)
  }

  def where[T](f: E => Column[T])(implicit wt: Query.WhereType[T]): Query[E] = new Query(value, f(value) :: cond, ordering)

  // Methods for use in for-comprehensions
  def flatMap[F <: AnyRef](f: E => Query[F]): Query[F] = select(f)
  def map[F <: AnyRef](f: E => F): Query[F] = flatMap(v => Query(f(v)))
  //def filter(f: E => BooleanColumn): Query[E] = where(f)

  def >>[F <: AnyRef](q: Query[F]): Query[F] = flatMap(_ => q)

  def nodeChildren = Node(value) :: cond.map(Node.apply) ::: ordering
  override def nodeNamedChildren = (Node(value), "select") :: cond.map(n => (Node(n), "where")) ::: ordering.map(o => (o, "order"))

  override def toString = "Query"
}

object Query {
  def apply[E <: AnyRef](value: E) = new Query(value, Nil, Nil)
  def apply[E <: AnyRef](value: E, cond: List[Column[Option[Boolean]]], ordering: List[Order.Ordering]) = new Query(value, cond, ordering)

  def unapply[E <: AnyRef](q: Query[E]) = Some((q.value, q.cond))

  def flatten[E <: AnyRef](q: Query[Query[E]]): Query[E] = q.flatMap(x => x)

  object QNothing
  
  trait WhereType[T]
}

class QueryOfColumnOps[E <: Column[_]](q: Query[E]) {
  
  def union(other: Query[E]): Query[E] = {
    val u = Union(false, q, other)
    Query(q.value.mapOp(n => Union.UnionPart(n, u)))
  }
}

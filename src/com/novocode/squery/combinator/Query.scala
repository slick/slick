package com.novocode.squery.combinator

import java.io.PrintWriter


class Query[+E <: AnyRef](val value: E, val cond: List[BooleanColumn]) extends Node {

  def select[F <: AnyRef](f: E => Query[F]): Query[F] = {
    val q = f(value)
    new Query(q.value, cond ::: q.cond)
  }

  def where(f: E => BooleanColumn): Query[E] = new Query(value, f(value) :: cond)

  // Methods for use in for-comprehensions
  def flatMap[F <: AnyRef](f: E => Query[F]): Query[F] = select(f)
  def map[F <: AnyRef](f: E => F): Query[F] = flatMap(v => Query(f(v)))
  //def filter(f: E => BooleanColumn): Query[E] = where(f)

  def nodeChildren = Node(value) :: cond.map(Node.apply)
  override def nodeChildrenNames = {
    lazy val s:Stream[String] = Stream.cons("where", s)
    Stream.cons("select", s)
  }

  override def toString = "Query"
}

object Query {
  def apply[E <: AnyRef](value: E) = new Query(value, Nil)
  def apply[E <: AnyRef](value: E, cond: List[BooleanColumn]) = new Query(value, cond)

  def unapply[E <: AnyRef](q: Query[E]) = Some((q.value, q.cond))

  def flatten[E <: AnyRef](q: Query[Query[E]]): Query[E] = q.flatMap(x => x)
}

class QueryOfColumnOps[E <: Column[_]](q: Query[E]) {
  
  def union(other: Query[E]): Query[E] =
    Query(q.value.withOp(Union.UnionPart(new Union(false, q, other))))
}

package com.novocode.squery.combinator

object Order {
  sealed abstract class Ordering private[Order] (val by: Node) extends Node {
    def nodeChildren = by :: Nil
  }

  final case class Asc private[Order] (override val by: Node) extends Ordering(by) {
    override def toString = "Order.Asc"
  }

  final case class Desc private[Order] (override val by: Node) extends Ordering(by) {
    override def toString = "Order.Desc"
  }

  def +(by: Column[_]) = new Query[Query.QNothing.type](Query.QNothing, Nil, Asc(Node(by)) :: Nil)

  def -(by: Column[_]) = new Query[Query.QNothing.type](Query.QNothing, Nil, Desc(Node(by)) :: Nil)
}

package com.novocode.squery.combinator

object OrderBy {
  sealed abstract class Ordering private[OrderBy] (val by: Node) extends Node {
    def nodeChildren = by :: Nil
  }

  final case class Asc private[OrderBy] (override val by: Node) extends Ordering(by) {
    override def toString = "OrderBy.Asc"
  }

  final case class Desc private[OrderBy] (override val by: Node) extends Ordering(by) {
    override def toString = "OrderBy.Desc"
  }

  def +(by: Column[_]) = new Query[Unit]((), Nil, Nil, Asc(Node(by)) :: Nil)

  def -(by: Column[_]) = new Query[Unit]((), Nil, Nil, Desc(Node(by)) :: Nil)
}

package com.novocode.squery.combinator

object GroupBy {
  final case class Grouping private[GroupBy] (val by: Node) extends Node {
    def nodeChildren = by :: Nil
    override def toString = "GroupBy.Grouping"
  }

  def apply(by: Column[_]) = new Query[Unit]((), Nil, Grouping(Node(by)) :: Nil, Nil)
}

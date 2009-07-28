package com.novocode.squery.combinator

final case class Grouping(val by: Node) extends Node {
  def nodeChildren = by :: Nil
  override def toString = "GroupBy.Grouping"
}

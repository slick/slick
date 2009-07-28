package com.novocode.squery.combinator

sealed abstract class Ordering(val by: Node) extends Node { def nodeChildren = by :: Nil }

object Ordering {
  final case class Asc(override val by: Node) extends Ordering(by) {
    override def toString = "OrderBy.Asc"
  }

  final case class Desc(override val by: Node) extends Ordering(by) {
    override def toString = "OrderBy.Desc"
  }
}

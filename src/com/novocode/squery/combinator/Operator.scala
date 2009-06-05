package com.novocode.squery.combinator

object Operator {
  final case class Is(left: Node, right: Node) extends BooleanColumn with BinaryNode
  final case class In(left: Node, right: Node) extends BooleanColumn with BinaryNode
  final case class And(left: Node, right: Node) extends BooleanColumn with BinaryNode
  final case class Or(left: Node, right: Node) extends BooleanColumn with BinaryNode
  final case class Count(child: Node) extends IntColumn with UnaryNode
  final case class Max(child: Node) extends IntColumn with UnaryNode
  final case class Not(child: Node) extends BooleanColumn with UnaryNode

  final case class Ordering(left: Node, right: Node, desc: Boolean) extends BinaryNode {
    override def toString = "Ordering " + (if(desc) "desc" else "asc")
    override def nodeChildrenNames = Stream("expr", "by")
  }
}

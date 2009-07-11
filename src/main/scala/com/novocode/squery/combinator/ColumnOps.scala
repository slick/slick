package com.novocode.squery.combinator

trait ColumnOps {
  protected[this] val leftOperand: Node
}

trait BooleanColumnOps extends ColumnOps {
  def &&(b: Column[Boolean]) = Operator.And(leftOperand, Node(b))
  def ||(b: Column[Boolean]) = Operator.Or(leftOperand, Node(b))
  def not = Operator.Not(leftOperand)
}

package com.novocode.squery.combinator

trait ColumnOps {
  protected val leftOperand: Node
}

trait BooleanLikeColumnOps extends ColumnOps {
  def &&(b: Column[Option[Boolean]]) = Operator.And(leftOperand, Node(b))
  def ||(b: Column[Option[Boolean]]) = Operator.Or(leftOperand, Node(b))
}

trait BooleanColumnOps extends ColumnOps {
  def unary_! = Operator.Not(leftOperand)
}

trait BooleanOptionColumnOps extends ColumnOps {
  def unary_! = Operator.Not(leftOperand).?
}

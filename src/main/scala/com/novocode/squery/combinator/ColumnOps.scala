package com.novocode.squery.combinator

trait ColumnOps {
  protected val leftOperand: Node
}

sealed trait OptionMapper[B, P1, P2, R] {
  def apply(n: Column[B]): Column[R]
}

object OptionMapper {
  val plain = new OptionMapper[Any,Any,Any,Any] { def apply(n: Column[Any]): Column[Any] = n }
  val option = new OptionMapper[Any,Any,Any,Option[Any]] { def apply(n: Column[Any]): Column[Option[Any]] = n.? }
}

trait BooleanColumnOps[P1] extends ColumnOps {
  def &&[P2, R](b: ColumnBase[P2])(implicit om: OptionMapper[Boolean, P1, P2, R]): Column[R] = om(Operator.And(leftOperand, Node(b)))
  def ||[P2, R](b: ColumnBase[P2])(implicit om: OptionMapper[Boolean, P1, P2, R]): Column[R] = om(Operator.Or(leftOperand, Node(b)))
  def unary_![R](implicit om: OptionMapper[Boolean, P1, P1, R]): Column[R] = om(Operator.Not(leftOperand))
}

package com.novocode.squery.combinator

trait ColumnOps {
  protected val leftOperand: Node
}

sealed trait OptionMapper[B, P1, P2, R] {
  def apply(n: SimpleColumn[B]): SimpleColumn[R]
}

object OptionMapper {
  val plain = new OptionMapper[Any,Any,Any,Any] { def apply(n: SimpleColumn[Any]): SimpleColumn[Any] = n }
  val option = new OptionMapper[Any,Any,Any,Option[Any]] { def apply(n: SimpleColumn[Any]): SimpleColumn[Option[Any]] = n.? }
}

trait BooleanColumnOps[P1] extends ColumnOps {
  def &&[P2, R](b: Column[P2])(implicit om: OptionMapper[Boolean, P1, P2, R]): SimpleColumn[R] = om(Operator.And(leftOperand, Node(b)))
  def ||[P2, R](b: Column[P2])(implicit om: OptionMapper[Boolean, P1, P2, R]): SimpleColumn[R] = om(Operator.Or(leftOperand, Node(b)))
  def unary_![R](implicit om: OptionMapper[Boolean, P1, P1, R]): SimpleColumn[R] = om(Operator.Not(leftOperand))
}

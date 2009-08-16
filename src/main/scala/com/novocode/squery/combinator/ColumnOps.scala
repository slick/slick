package com.novocode.squery.combinator

trait ColumnOps {
  protected val leftOperand: Node
}

sealed trait OptionMapper[B1, B2, BR, P1, P2, R] extends (Column[BR] => Column[R])

object OptionMapper {
  val plain = new OptionMapper[Any,Any,Any,Any,Any,Any] { def apply(n: Column[Any]): Column[Any] = n }
  val option = new OptionMapper[Any,Any,Any,Any,Any,Option[Any]] { def apply(n: Column[Any]): Column[Option[Any]] = n.? }
}

trait AllColumnOps[B1, P1] extends ColumnOps {
  def is[B2, P2, R](e: Column[P2])(implicit om: OptionMapper[B1, B2, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Is(leftOperand, Node(e)))
  def isNot[B2, P2, R](e: Column[P2])(implicit om: OptionMapper[B1, B2, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Not(Operator.Is(leftOperand, Node(e))))
  def < [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Relational("<", leftOperand, Node(e)))
  def <= [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Relational("<=", leftOperand, Node(e)))
  def > [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Relational(">", leftOperand, Node(e)))
  def >= [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Relational(">=", leftOperand, Node(e)))
  def inSet[R](seq: Seq[B1])(implicit om: OptionMapper[B1, B1, Boolean, P1, P1, R], tm: BaseTypeMapper[B1]): Column[R] =
    om(Operator.InSet(leftOperand, seq, tm, false))
  def inSetBind[R](seq: Seq[B1])(implicit om: OptionMapper[B1, B1, Boolean, P1, P1, R], tm: BaseTypeMapper[B1]): Column[R] =
    om(Operator.InSet(leftOperand, seq, tm, true))

  // NumericTypeMapper only
  def + [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Operator.Arith[B1]("+", leftOperand, Node(e), tm))
  def - [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Operator.Arith[B1]("-", leftOperand, Node(e), tm))
  def * [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Operator.Arith[B1]("*", leftOperand, Node(e), tm))
  def / [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Operator.Arith[B1]("/", leftOperand, Node(e), tm))
}

trait BooleanColumnOps[P1] extends ColumnOps {
  def &&[P2, R](b: ColumnBase[P2])(implicit om: OptionMapper[Boolean, Boolean, Boolean, P1, P2, R]): Column[R] =
    om(Operator.And(leftOperand, Node(b)))
  def ||[P2, R](b: ColumnBase[P2])(implicit om: OptionMapper[Boolean, Boolean, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Or(leftOperand, Node(b)))
  def unary_![R](implicit om: OptionMapper[Boolean, Boolean, Boolean, P1, P1, R]): Column[R] =
    om(Operator.Not(leftOperand))
}

trait StringColumnOps[P1] extends ColumnOps {
  def length[R](implicit om: OptionMapper[String, String, Int, P1, P1, R]): Column[R] =
    om(Operator.Length(leftOperand))
  def like[P2, R](e: Column[P2])(implicit om: OptionMapper[String, String, Boolean, P1, P2, R]): Column[R] =
    om(Operator.Like(leftOperand, Node(e)))
}

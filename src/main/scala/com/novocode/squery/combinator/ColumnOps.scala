package com.novocode.squery.combinator

import TypeMapper._

trait ColumnOps {
  protected val leftOperand: Node
}

trait AllColumnOps[B1, P1] extends ColumnOps {
  import AllColumnOps._
  def is[B2, P2, R](e: Column[P2])(implicit om: OptionMapper2[B1, B2, Boolean, P1, P2, R]): Column[R] =
    om(Is(leftOperand, Node(e)))
  def isNot[B2, P2, R](e: Column[P2])(implicit om: OptionMapper2[B1, B2, Boolean, P1, P2, R]): Column[R] =
    om(BooleanColumnOps.Not(Is(leftOperand, Node(e))))
  def < [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Relational("<", leftOperand, Node(e)))
  def <= [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Relational("<=", leftOperand, Node(e)))
  def > [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Relational(">", leftOperand, Node(e)))
  def >= [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, Boolean, P1, P2, R]): Column[R] =
    om(Relational(">=", leftOperand, Node(e)))
  def inSet[R](seq: Seq[B1])(implicit om: OptionMapper2[B1, B1, Boolean, P1, P1, R], tm: BaseTypeMapper[B1]): Column[R] =
    om(InSet(leftOperand, seq, tm, false))
  def inSetBind[R](seq: Seq[B1])(implicit om: OptionMapper2[B1, B1, Boolean, P1, P1, R], tm: BaseTypeMapper[B1]): Column[R] =
    om(InSet(leftOperand, seq, tm, true))
  def between[P2, P3, R](start: Column[P2], end: Column[P3])(implicit om: OptionMapper3[B1, B1, B1, Boolean, P1, P2, P3, R]): Column[R] =
    om(Between(leftOperand, start, end))

  // NumericTypeMapper only
  def + [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Arith[B1]("+", leftOperand, Node(e), tm))
  def - [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Arith[B1]("-", leftOperand, Node(e), tm))
  def * [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Arith[B1]("*", leftOperand, Node(e), tm))
  def / [P2, R](e: ColumnBase[P2])(implicit om: OptionMapper2[B1, B1, B1, P1, P2, R], tm: BaseTypeMapper[B1] with NumericTypeMapper): Column[R] =
    om(Arith[B1]("/", leftOperand, Node(e), tm))
}

object AllColumnOps {
  case class In(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "in" }
  case class Count(child: Node) extends OperatorColumn[Int] with SimpleFunction with UnaryNode { val name = "count" }
  case class CountAll(child: Node) extends OperatorColumn[Int] with UnaryNode
  case class Avg(child: Node) extends SimpleFunction with UnaryNode { val name = "avg" }
  case class Min(child: Node) extends SimpleFunction with UnaryNode { val name = "min" }
  case class Max(child: Node) extends SimpleFunction with UnaryNode { val name = "max" }
  case class Relational(name: String, left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean]
  case class Exists(child: Node) extends OperatorColumn[Boolean] with SimpleFunction with UnaryNode with BooleanColumnOps[Boolean] { val name = "exists" }
  case class Arith[T](name: String, left: Node, right: Node, tm: TypeMapper[T]) extends OperatorColumn[T]()(tm) with SimpleBinaryOperator

  case class Is(left: Node, right: Node) extends OperatorColumn[Boolean] with BinaryNode with BooleanColumnOps[Boolean]
  case class CountDistinct(child: Node) extends OperatorColumn[Int] with UnaryNode
  case class InSet[T](child: Node, seq: Seq[T], tm: TypeMapper[T], bind: Boolean) extends OperatorColumn[Boolean] with UnaryNode

  case class Between(left: Node, start: Node, end: Node) extends OperatorColumn[Boolean] with BooleanColumnOps[Boolean] {
    def nodeChildren = left :: start :: end :: Nil
  }
}

trait BooleanColumnOps[P1] extends ColumnOps {
  import BooleanColumnOps._
  def &&[P2, R](b: ColumnBase[P2])(implicit om: OptionMapper2[Boolean, Boolean, Boolean, P1, P2, R]): Column[R] =
    om(And(leftOperand, Node(b)))
  def ||[P2, R](b: ColumnBase[P2])(implicit om: OptionMapper2[Boolean, Boolean, Boolean, P1, P2, R]): Column[R] =
    om(Or(leftOperand, Node(b)))
  def unary_![R](implicit om: OptionMapper2[Boolean, Boolean, Boolean, P1, P1, R]): Column[R] =
    om(Not(leftOperand))
}

object BooleanColumnOps {
  case class And(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "and" }
  case class Or(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "or" }
  case class Not(child: Node) extends OperatorColumn[Boolean] with UnaryNode with BooleanColumnOps[Boolean]
}

trait StringColumnOps[P1] extends ColumnOps {
  import StringColumnOps._
  def length[R](implicit om: OptionMapper2[String, String, Int, P1, P1, R]): Column[R] =
    om(Length(leftOperand))
  def like[P2, R](e: Column[P2])(implicit om: OptionMapper2[String, String, Boolean, P1, P2, R]): Column[R] =
    om(Like(leftOperand, Node(e)))
}

object StringColumnOps {
  case class Length(child: Node) extends OperatorColumn[Int] with SimpleFunction with UnaryNode { val name = "length" }
  case class Like(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "like" }
}

package com.novocode.squery.combinator

import com.novocode.squery.session.TypeMapper
import com.novocode.squery.session.TypeMapper._

object Operator {
  case class In(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "in" }
  case class Like(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "like" }
  case class And(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "and" }
  case class Or(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "or" }
  case class Count(child: Node) extends OperatorColumn[Int] with SimpleFunction with UnaryNode { val name = "count" }
  case class Avg(child: Node) extends SimpleFunction with UnaryNode { val name = "avg" }
  case class Min(child: Node) extends SimpleFunction with UnaryNode { val name = "min" }
  case class Max(child: Node) extends SimpleFunction with UnaryNode { val name = "max" }
  case class Relational(name: String, left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean]
  case class Length(child: Node) extends OperatorColumn[Int] with SimpleFunction with UnaryNode { val name = "length" }
  case class Exists(child: Node) extends OperatorColumn[Boolean] with SimpleFunction with UnaryNode with BooleanColumnOps[Boolean] { val name = "exists" }
  case class Arith[T](name: String, left: Node, right: Node, tm: TypeMapper[T]) extends OperatorColumn[T]()(tm) with SimpleBinaryOperator

  case class Is(left: Node, right: Node) extends OperatorColumn[Boolean] with BinaryNode with BooleanColumnOps[Boolean]
  case class Not(child: Node) extends OperatorColumn[Boolean] with UnaryNode with BooleanColumnOps[Boolean]
  case class CountDistinct(child: Node) extends OperatorColumn[Int] with UnaryNode
  case class InSet[T](child: Node, seq: Seq[T], tm: TypeMapper[T], bind: Boolean) extends OperatorColumn[Boolean] with UnaryNode
}

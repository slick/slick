package com.novocode.squery.combinator

import com.novocode.squery.session.TypeMapper
import com.novocode.squery.session.TypeMapper._

object Operator {
  final case class In(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "in" }
  final case class Like(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "like" }
  final case class And(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "and" }
  final case class Or(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] { val name = "or" }
  final case class Count(child: Node) extends OperatorColumn[Int] with SimpleFunction with UnaryNode { val name = "count" }
  final case class Avg(child: Node) extends SimpleFunction with UnaryNode { val name = "avg" }
  final case class Min(child: Node) extends SimpleFunction with UnaryNode { val name = "min" }
  final case class Max(child: Node) extends SimpleFunction with UnaryNode { val name = "max" }
  final case class Relational(name: String, left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean]
  final case class Length(child: Node) extends OperatorColumn[Int] with SimpleFunction with UnaryNode { val name = "length" }
  final case class Exists(child: Node) extends OperatorColumn[Boolean] with SimpleFunction with UnaryNode with BooleanColumnOps[Boolean] { val name = "exists" }
  final case class Arith[T](name: String, left: Node, right: Node, tm: TypeMapper[T]) extends OperatorColumn[T]()(tm) with SimpleBinaryOperator

  final case class Is(left: Node, right: Node) extends OperatorColumn[Boolean] with BinaryNode with BooleanColumnOps[Boolean]
  final case class Not(child: Node) extends OperatorColumn[Boolean] with UnaryNode with BooleanColumnOps[Boolean]
  final case class CountDistinct(child: Node) extends OperatorColumn[Int] with UnaryNode
  final case class InSet[T](child: Node, seq: Seq[T], tm: TypeMapper[T], bind: Boolean) extends OperatorColumn[Boolean] with UnaryNode
}

package com.novocode.squery.combinator

import com.novocode.squery.session.TypeMapper
import com.novocode.squery.session.TypeMapper._

object Operator {
  final case class In(left: Node, right: Node) extends SimpleBinaryOperator[Boolean] with BooleanColumnOps[Boolean] { val name = "in" }
  final case class And(left: Node, right: Node) extends SimpleBinaryOperator[Boolean] with BooleanColumnOps[Boolean] { val name = "and" }
  final case class Or(left: Node, right: Node) extends SimpleBinaryOperator[Boolean] with BooleanColumnOps[Boolean] { val name = "or" }
  final case class Count(child: Node) extends SimpleFunction[Int] with UnaryNode { val name = "count" }
  final case class Max(child: Node) extends SimpleFunction[Int] with UnaryNode { val name = "max" }

  final case class Is(left: Node, right: Node) extends OperatorColumn[Boolean] with BinaryNode with BooleanColumnOps[Boolean]
  final case class Not(child: Node) extends OperatorColumn[Boolean] with UnaryNode with BooleanColumnOps[Boolean]
}

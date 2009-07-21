package com.novocode.squery.combinator

trait SimpleFunction[T] extends OperatorColumn[T] {
  val name: String
}

trait SimpleBinaryOperator[T] extends OperatorColumn[T] with BinaryNode {
  val name: String
}

case class CustomFunction[T](name: String)(paramsC: Column[_]*) extends SimpleFunction[T] {
  def nodeChildren = paramsC.map(n => Node(n)).toList
}

case class CustomBinaryOperator[T](name: String)(leftC: Column[_], rightC: Column[_]) extends SimpleBinaryOperator[T] {
  val left = Node(leftC)
  val right = Node(rightC)
}

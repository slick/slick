package com.novocode.squery.combinator

import com.novocode.squery.session.TypeMapper

abstract class SimpleFunction[T](implicit tm: TypeMapper[T]) extends OperatorColumn[T]()(tm) {
  val name: String
}

object SimpleFunction {
  def apply[T](fname: String)(implicit tm: TypeMapper[T]): (Seq[Column[_]] => SimpleFunction[T]) =
    (paramsC: Seq[Column[_]]) =>
      new SimpleFunction[T]()(tm) {
        val name = fname
        def nodeChildren = paramsC.map(n => Node(n)).toList
      }
}

abstract class SimpleBinaryOperator[T](implicit tm: TypeMapper[T]) extends OperatorColumn[T]()(tm) with BinaryNode {
  val name: String
}

object SimpleBinaryOperator {
  def apply[T](fname: String)(implicit tm: TypeMapper[T]): ((Column[_], Column[_]) => SimpleBinaryOperator[T]) =
    (leftC: Column[_], rightC: Column[_]) =>
      new SimpleBinaryOperator[T]()(tm) {
        val name = fname
        val left = Node(leftC)
        val right = Node(rightC)
      }
}

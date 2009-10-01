package com.novocode.squery.combinator

trait SimpleFunction extends Node {
  val name: String
}

object SimpleFunction {
  def apply[T : TypeMapper](fname: String): (Seq[Column[_]] => OperatorColumn[T] with SimpleFunction) =
    (paramsC: Seq[Column[_]]) =>
      new OperatorColumn[T] with SimpleFunction {
        val name = fname
        def nodeChildren = paramsC.map(n => Node(n)).toList
      }
}

trait SimpleScalarFunction extends Node {
  val name: String
}

object SimpleScalarFunction {
  def apply[T : TypeMapper](fname: String): (Seq[Column[_]] => OperatorColumn[T] with SimpleScalarFunction) =
    (paramsC: Seq[Column[_]]) =>
      new OperatorColumn[T] with SimpleScalarFunction {
        val name = fname
        def nodeChildren = paramsC.map(n => Node(n)).toList
      }
  def nullary[T : TypeMapper](fnName: String) =
    new OperatorColumn[T] with SimpleScalarFunction with NullaryNode { val name = fnName }
}

trait SimpleBinaryOperator extends BinaryNode {
  val name: String
}

object SimpleBinaryOperator {
  def apply[T : TypeMapper](fname: String): ((Column[_], Column[_]) => OperatorColumn[T] with SimpleBinaryOperator) =
    (leftC: Column[_], rightC: Column[_]) =>
      new OperatorColumn[T] with SimpleBinaryOperator {
        val name = fname
        val left = Node(leftC)
        val right = Node(rightC)
      }
}

case class SimpleLiteral(name: String) extends Node {
  val nodeChildren = Nil
}

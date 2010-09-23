package org.scalaquery.ql

import org.scalaquery.util.{Node, UnaryNode, NullaryNode, BinaryNode}

/**
 * A SimpleFunction gets translated to a plain function call in SQL.
 */
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

/**
 * A SimpleScalarFunction gets translated to a JDBC/ODBC {fn ...} call.
 */
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
  def unapply(s: SimpleScalarFunction) = Some(s.name)
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

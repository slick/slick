package org.scalaquery.ql

import org.scalaquery.util.{Node, UnaryNode, NullaryNode, BinaryNode}

/**
 * A SimpleFunction gets translated to a plain function call or JDBC/ODBC
 * scalar function {fn ...} call in SQL.
 */
trait SimpleFunction extends Node {
  val name: String
  val scalar = false
}

object SimpleFunction {
  def apply[T : TypeMapper](fname: String, fn: Boolean = false): (Seq[Column[_]] => OperatorColumn[T] with SimpleFunction) =
    (paramsC: Seq[Column[_]]) =>
      new OperatorColumn[T] with SimpleFunction {
        val name = fname
        override val scalar = fn
        def nodeChildren = paramsC.map(n => Node(n)).toList
      }
  def nullary[R : TypeMapper](fname: String, fn: Boolean = false): OperatorColumn[R] with SimpleFunction =
    apply(fname, fn).apply(Seq())
  def unary[T1, R : TypeMapper](fname: String, fn: Boolean = false): (Column[T1] => OperatorColumn[R] with SimpleFunction) = {
    val f = apply(fname, fn);
    { t1: Column[T1] => f(Seq(t1)) }
  }
  def binary[T1, T2, R : TypeMapper](fname: String, fn: Boolean = false): ((Column[T1], Column[T2]) => OperatorColumn[R] with SimpleFunction) = {
    val f = apply(fname, fn);
    { (t1: Column[T1], t2: Column[T2]) => f(Seq(t1, t2)) }
  }
  def ternary[T1, T2, T3, R : TypeMapper](fname: String, fn: Boolean = false): ((Column[T1], Column[T2], Column[T3]) => OperatorColumn[R] with SimpleFunction) = {
    val f = apply(fname, fn);
    { (t1: Column[T1], t2: Column[T2], t3: Column[T3]) => f(Seq(t1, t2, t3)) }
  }
  def unapply(s: SimpleFunction) = Some((s.name, s.scalar))
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

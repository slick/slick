package scala.slick.ql

import scala.slick.driver.BasicStatementBuilderComponent
import scala.slick.ast._
import scala.slick.util._

/**
 * A SimpleFunction gets translated to a plain function call or JDBC/ODBC
 * scalar function {fn ...} call in SQL.
 */
trait SimpleFunction extends SimpleNode {
  val name: String
  val scalar = false
  override def toString = "SimpleFunction(" + name + ", " + scalar + ")"
}

object SimpleFunction {
  def apply[T : TypeMapper](fname: String, fn: Boolean = false): (Seq[Column[_]] => OperatorColumn[T] with SimpleFunction) = {
    lazy val builder: (Seq[NodeGenerator] => OperatorColumn[T] with SimpleFunction) = paramsC =>
      new OperatorColumn[T] with SimpleFunction {
        val name = fname
        override val scalar = fn
        protected[this] def nodeChildGenerators = paramsC
        protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = builder(ch)
      }
    builder
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
}

case class StdFunction[T : TypeMapper](name: String, children: Node*) extends OperatorColumn[T] with SimpleFunction {
  protected[this] def nodeChildGenerators = children
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = StdFunction(name, ch: _*)
}

case class EscFunction[T : TypeMapper](name: String, children: Node*) extends OperatorColumn[T] with SimpleFunction {
  protected[this] def nodeChildGenerators = children
  override val scalar = true
  protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = EscFunction(name, ch: _*)
}

trait SimpleBinaryOperator extends BinaryNode {
  val name: String
}

object SimpleBinaryOperator {
  def apply[T : TypeMapper](fname: String): ((Column[_], Column[_]) => OperatorColumn[T] with SimpleBinaryOperator) = {
    lazy val builder: ((NodeGenerator, NodeGenerator) => OperatorColumn[T] with SimpleBinaryOperator) = (leftC, rightC) =>
      new OperatorColumn[T] with SimpleBinaryOperator {
        val name = fname
        val left = Node(leftC)
        val right = Node(rightC)
        protected[this] def nodeRebuild(left: Node, right: Node): Node = builder(left, right)
      }
    builder
  }
}

case class SimpleLiteral(name: String) extends NullaryNode

trait SimpleExpression extends SimpleNode {
  def toSQL(qb: BasicStatementBuilderComponent#QueryBuilder): Unit
}

object SimpleExpression {
  def apply[T : TypeMapper](f: (Seq[Node], BasicStatementBuilderComponent#QueryBuilder) => Unit): (Seq[Column[_]] => OperatorColumn[T] with SimpleExpression) = {
    lazy val builder: (Seq[NodeGenerator] => OperatorColumn[T] with SimpleExpression) = paramsC =>
      new OperatorColumn[T] with SimpleExpression {
        def toSQL(qb: BasicStatementBuilderComponent#QueryBuilder) = f(nodeChildren.toSeq, qb)
        protected[this] def nodeChildGenerators = paramsC
        protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = builder(ch)
      }
    builder
  }

  def nullary[R : TypeMapper](f: BasicStatementBuilderComponent#QueryBuilder => Unit): OperatorColumn[R] with SimpleExpression = {
    val g = apply({ (ch: Seq[Node], qb: BasicStatementBuilderComponent#QueryBuilder) => f(qb) });
    g.apply(Seq())
  }
  
  def unary[T1, R : TypeMapper](f: (Node, BasicStatementBuilderComponent#QueryBuilder) => Unit): (Column[T1] => OperatorColumn[R] with SimpleExpression) = {
    val g = apply({ (ch: Seq[Node], qb: BasicStatementBuilderComponent#QueryBuilder) => f(ch(0), qb) });
    { t1: Column[T1] => g(Seq(t1)) }
  }

  def binary[T1, T2, R : TypeMapper](f: (Node, Node, BasicStatementBuilderComponent#QueryBuilder) => Unit): ((Column[T1], Column[T2]) => OperatorColumn[R] with SimpleExpression) = {
    val g = apply({ (ch: Seq[Node], qb: BasicStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), qb) });
    { (t1: Column[T1], t2: Column[T2]) => g(Seq(t1, t2)) }
  }

  def ternary[T1, T2, T3, R : TypeMapper](f: (Node, Node, Node, BasicStatementBuilderComponent#QueryBuilder) => Unit): ((Column[T1], Column[T2], Column[T3]) => OperatorColumn[R] with SimpleExpression) = {
    val g = apply({ (ch: Seq[Node], qb: BasicStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), ch(2), qb) });
    { (t1: Column[T1], t2: Column[T2], t3: Column[T3]) => g(Seq(t1, t2, t3)) }
  }
}

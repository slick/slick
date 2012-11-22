package scala.slick.lifted

import scala.slick.driver.JdbcStatementBuilderComponent
import scala.slick.ast._
import scala.slick.util._

/**
 * A SimpleFunction gets translated to a plain function call or JDBC/ODBC
 * scalar function {fn ...} call in SQL.
 */
trait SimpleFunction extends Node {
  val name: String
  val scalar = false
  override def toString = "SimpleFunction(" + name + ", " + scalar + ")"
}

object SimpleFunction {
  def apply[T : TypedType](fname: String, fn: Boolean = false): (Seq[Column[_]] => Column[T] with SimpleFunction) = {
    lazy val builder: (Seq[NodeGenerator] => Column[T] with SimpleFunction) = paramsC =>
      new Column[T] with SimpleFunction with TypedNode {
        val name = fname
        override val scalar = fn
        lazy val nodeChildren = paramsC.map(Node(_))
        protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = builder(ch)
      }
    builder
  }
  def nullary[R : TypedType](fname: String, fn: Boolean = false): Column[R] with SimpleFunction =
    apply(fname, fn).apply(Seq())
  def unary[T1, R : TypedType](fname: String, fn: Boolean = false): (Column[T1] => Column[R] with SimpleFunction) = {
    val f = apply(fname, fn);
    { t1: Column[T1] => f(Seq(t1)) }
  }
  def binary[T1, T2, R : TypedType](fname: String, fn: Boolean = false): ((Column[T1], Column[T2]) => Column[R] with SimpleFunction) = {
    val f = apply(fname, fn);
    { (t1: Column[T1], t2: Column[T2]) => f(Seq(t1, t2)) }
  }
  def ternary[T1, T2, T3, R : TypedType](fname: String, fn: Boolean = false): ((Column[T1], Column[T2], Column[T3]) => Column[R] with SimpleFunction) = {
    val f = apply(fname, fn);
    { (t1: Column[T1], t2: Column[T2], t3: Column[T3]) => f(Seq(t1, t2, t3)) }
  }
}

trait SimpleBinaryOperator extends BinaryNode {
  val name: String
}

object SimpleBinaryOperator {
  def apply[T : TypedType](fname: String): ((Column[_], Column[_]) => Column[T] with SimpleBinaryOperator) = {
    lazy val builder: ((NodeGenerator, NodeGenerator) => Column[T] with SimpleBinaryOperator) = (leftC, rightC) =>
      new Column[T] with SimpleBinaryOperator with TypedNode {
        val name = fname
        val left = Node(leftC)
        val right = Node(rightC)
        protected[this] def nodeRebuild(left: Node, right: Node): Node = builder(left, right)
      }
    builder
  }
}

case class SimpleLiteral(name: String)(val tpe: Type) extends NullaryNode with TypedNode {
  def nodeRebuild = copy()(tpe)
}

trait SimpleExpression extends Node {
  def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder): Unit
}

object SimpleExpression {
  def apply[T : TypedType](f: (Seq[Node], JdbcStatementBuilderComponent#QueryBuilder) => Unit): (Seq[Column[_]] => Column[T] with SimpleExpression) = {
    lazy val builder: (Seq[NodeGenerator] => Column[T] with SimpleExpression) = paramsC =>
      new Column[T] with SimpleExpression with TypedNode {
        def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder) = f(nodeChildren.toSeq, qb)
        val nodeChildren = paramsC.map(Node(_))
        protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = builder(ch)
      }
    builder
  }

  def nullary[R : TypedType](f: JdbcStatementBuilderComponent#QueryBuilder => Unit): Column[R] with SimpleExpression = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(qb) });
    g.apply(Seq())
  }
  
  def unary[T1, R : TypedType](f: (Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): (Column[T1] => Column[R] with SimpleExpression) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), qb) });
    { t1: Column[T1] => g(Seq(t1)) }
  }

  def binary[T1, T2, R : TypedType](f: (Node, Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): ((Column[T1], Column[T2]) => Column[R] with SimpleExpression) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), qb) });
    { (t1: Column[T1], t2: Column[T2]) => g(Seq(t1, t2)) }
  }

  def ternary[T1, T2, T3, R : TypedType](f: (Node, Node, Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): ((Column[T1], Column[T2], Column[T3]) => Column[R] with SimpleExpression) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), ch(2), qb) });
    { (t1: Column[T1], t2: Column[T2], t3: Column[T3]) => g(Seq(t1, t2, t3)) }
  }
}

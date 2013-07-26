package scala.slick.lifted

import scala.slick.driver.JdbcStatementBuilderComponent
import scala.slick.ast._
import scala.slick.util._

/** Base class for SimpleFunction/BinaryOperator/Expression implementations. */
private[lifted] abstract class SimpleFeatureNode[T](implicit val tpe: TypedType[T]) extends TypedNode {
  type Self = SimpleFeatureNode[T]
}

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
  def apply[T : TypedType](fname: String, fn: Boolean = false): (Seq[Column[_]] => Column[T]) = {
    def build(params: IndexedSeq[Node]): SimpleFeatureNode[T] = new SimpleFeatureNode[T] with SimpleFunction {
      val name = fname
      override val scalar = fn
      def nodeChildren = params
      protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = build(ch)
    }
    { paramsC: Seq[Column[_] ] => Column.forNode(build(paramsC.map(_.toNode)(collection.breakOut))) }
  }
  def nullary[R : TypedType](fname: String, fn: Boolean = false): Column[R] =
    apply(fname, fn).apply(Seq())
  def unary[T1, R : TypedType](fname: String, fn: Boolean = false): (Column[T1] => Column[R]) = {
    val f = apply(fname, fn);
    { t1: Column[T1] => f(Seq(t1)) }
  }
  def binary[T1, T2, R : TypedType](fname: String, fn: Boolean = false): ((Column[T1], Column[T2]) => Column[R]) = {
    val f = apply(fname, fn);
    { (t1: Column[T1], t2: Column[T2]) => f(Seq(t1, t2)) }
  }
  def ternary[T1, T2, T3, R : TypedType](fname: String, fn: Boolean = false): ((Column[T1], Column[T2], Column[T3]) => Column[R]) = {
    val f = apply(fname, fn);
    { (t1: Column[T1], t2: Column[T2], t3: Column[T3]) => f(Seq(t1, t2, t3)) }
  }
}

trait SimpleBinaryOperator extends BinaryNode {
  val name: String
}

object SimpleBinaryOperator {
  def apply[T : TypedType](fname: String): ((Column[_], Column[_]) => Column[T]) = {
    def build(leftN: Node, rightN: Node): SimpleFeatureNode[T] = new SimpleFeatureNode[T] with SimpleBinaryOperator {
      val name = fname
      val left = leftN
      val right = rightN
      protected[this] def nodeRebuild(left: Node, right: Node): Self = build(left, right)
    }
    { (leftC: Column[_], rightC: Column[_]) => Column.forNode[T](build(leftC.toNode, rightC.toNode)) }
  }
}

final case class SimpleLiteral(name: String)(val tpe: Type) extends NullaryNode with TypedNode {
  type Self = SimpleLiteral
  def nodeRebuild = copy()(tpe)
}

trait SimpleExpression extends Node {
  def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder): Unit
}

object SimpleExpression {
  def apply[T : TypedType](f: (Seq[Node], JdbcStatementBuilderComponent#QueryBuilder) => Unit): (Seq[Column[_]] => Column[T]) = {
    def build(params: IndexedSeq[Node]): SimpleFeatureNode[T] = new SimpleFeatureNode[T] with SimpleExpression {
      def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder) = f(nodeChildren, qb)
      def nodeChildren = params
      protected[this] def nodeRebuild(ch: IndexedSeq[Node]) = build(ch)
    }
    { paramsC: Seq[Column[_] ] => Column.forNode(build(paramsC.map(_.toNode)(collection.breakOut))) }
  }

  def nullary[R : TypedType](f: JdbcStatementBuilderComponent#QueryBuilder => Unit): Column[R] = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(qb) });
    g.apply(Seq())
  }
  
  def unary[T1, R : TypedType](f: (Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): (Column[T1] => Column[R]) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), qb) });
    { t1: Column[T1] => g(Seq(t1)) }
  }

  def binary[T1, T2, R : TypedType](f: (Node, Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): ((Column[T1], Column[T2]) => Column[R]) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), qb) });
    { (t1: Column[T1], t2: Column[T2]) => g(Seq(t1, t2)) }
  }

  def ternary[T1, T2, T3, R : TypedType](f: (Node, Node, Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): ((Column[T1], Column[T2], Column[T3]) => Column[R]) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), ch(2), qb) });
    { (t1: Column[T1], t2: Column[T2], t3: Column[T3]) => g(Seq(t1, t2, t3)) }
  }
}
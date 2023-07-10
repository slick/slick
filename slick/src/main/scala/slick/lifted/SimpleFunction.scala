package slick.lifted

import slick.ast.*
import slick.jdbc.JdbcStatementBuilderComponent
import slick.util.*

/** Base class for SimpleFunction/BinaryOperator/Expression implementations. */
private[lifted] abstract class SimpleFeatureNode[T](implicit val buildType: TypedType[T]) extends SimplyTypedNode {
  type Self = SimpleFeatureNode[T]
}

/** A SimpleFunction gets translated to a plain function call or JDBC/ODBC
  * scalar function {fn ...} call in SQL. */
trait SimpleFunction extends Node {
  val name: String
  val scalar = false
  override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"$name, $scalar")
}

object SimpleFunction {
  def apply[T : TypedType](fname: String, fn: Boolean = false): (Seq[Rep[_]] => Rep[T]) = {
    def build(params: IndexedSeq[Node]): SimpleFeatureNode[T] = new SimpleFeatureNode[T] with SimpleFunction {
      override def self = this
      val name = fname
      override val scalar = fn
      def children = ConstArray.from(params)
      protected[this] def rebuild(ch: ConstArray[Node]): Self = build(ch.toSeq)
    }
    { (paramsC: Seq[Rep[_]]) => Rep.forNode(build(paramsC.iterator.map(_.toNode).toIndexedSeq)) }
  }
  def nullary[R : TypedType](fname: String, fn: Boolean = false): Rep[R] =
    apply(fname, fn).apply(Seq())
  def unary[T1, R : TypedType](fname: String, fn: Boolean = false): (Rep[T1] => Rep[R]) = {
    val f = apply(fname, fn);
    { (t1: Rep[T1]) => f(Seq(t1)) }
  }
  def binary[T1, T2, R : TypedType](fname: String, fn: Boolean = false): ((Rep[T1], Rep[T2]) => Rep[R]) = {
    val f = apply(fname, fn);
    { (t1: Rep[T1], t2: Rep[T2]) => f(Seq(t1, t2)) }
  }
  def ternary[T1, T2, T3, R : TypedType](fname: String, fn: Boolean = false): ((Rep[T1], Rep[T2], Rep[T3]) => Rep[R]) = {
    val f = apply(fname, fn);
    { (t1: Rep[T1], t2: Rep[T2], t3: Rep[T3]) => f(Seq(t1, t2, t3)) }
  }
}

/** A SimpleBinaryOperator gets translated to a binary operator call in SQL. */
trait SimpleBinaryOperator extends BinaryNode {
  val name: String
}

object SimpleBinaryOperator {
  def apply[T : TypedType](fname: String): ((Rep[_], Rep[_]) => Rep[T]) = {
    def build(leftN: Node, rightN: Node): SimpleFeatureNode[T] = new SimpleFeatureNode[T] with SimpleBinaryOperator {
      override def self = this
      val name = fname
      val left = leftN
      val right = rightN
      protected[this] def rebuild(left: Node, right: Node): Self = build(left, right)
    }
    { (leftC: Rep[_], rightC: Rep[_]) => Rep.forNode[T](build(leftC.toNode, rightC.toNode)) }
  }
}

/** A SimpleLiteral is inserted verbatim into a SQL query string. For the
  * purpose of handling it in the query compiler it is assumed to be an
  * expression of the specified type. */
final case class SimpleLiteral(name: String)(val buildType: Type) extends NullaryNode with SimplyTypedNode {
  type Self = SimpleLiteral
  override def self = this
  def rebuild = copy()(buildType)
}
object SimpleLiteral{
  def apply[T](name: String)(implicit tpe: TypedType[T]) = Rep.forNode[T](new SimpleLiteral(name)(tpe))
}
/** A SimpleExpression allows arbitrary SQL code to be generated. */
trait SimpleExpression extends Node {
  def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder): Unit
}

object SimpleExpression {
  def apply[T : TypedType](f: (Seq[Node], JdbcStatementBuilderComponent#QueryBuilder) => Unit): (Seq[Rep[_]] => Rep[T]) = {
    def build(params: IndexedSeq[Node]): SimpleFeatureNode[T] = new SimpleFeatureNode[T] with SimpleExpression {
      override def self = this
      def toSQL(qb: JdbcStatementBuilderComponent#QueryBuilder) = f(children.toSeq, qb)
      def children = ConstArray.from(params)
      protected[this] def rebuild(ch: ConstArray[Node]) = build(ch.toSeq)
    }
    { (paramsC: Seq[Rep[_]]) => Rep.forNode(build(paramsC.iterator.map(_.toNode).toIndexedSeq)) }
  }

  def nullary[R : TypedType](f: JdbcStatementBuilderComponent#QueryBuilder => Unit): Rep[R] = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(qb) });
    g.apply(Seq())
  }
  
  def unary[T1, R : TypedType](f: (Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): (Rep[T1] => Rep[R]) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), qb) });
    { (t1: Rep[T1]) => g(Seq(t1)) }
  }

  def binary[T1, T2, R : TypedType](f: (Node, Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): ((Rep[T1], Rep[T2]) => Rep[R]) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), qb) });
    { (t1: Rep[T1], t2: Rep[T2]) => g(Seq(t1, t2)) }
  }

  def ternary[T1, T2, T3, R : TypedType](f: (Node, Node, Node, JdbcStatementBuilderComponent#QueryBuilder) => Unit): ((Rep[T1], Rep[T2], Rep[T3]) => Rep[R]) = {
    val g = apply({ (ch: Seq[Node], qb: JdbcStatementBuilderComponent#QueryBuilder) => f(ch(0), ch(1), ch(2), qb) });
    { (t1: Rep[T1], t2: Rep[T2], t3: Rep[T3]) => g(Seq(t1, t2, t3)) }
  }
}

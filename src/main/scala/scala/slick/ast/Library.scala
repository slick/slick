package scala.slick.ast

import scala.slick.ql.{OperatorColumn, TypeMapper}

/**
 * The standard library for query operators.
 */
object Library {
  class JdbcFunction(name: String) extends FunctionSymbol(name)
  class SqlFunction(name: String) extends FunctionSymbol(name)
  class SqlOperator(name: String) extends FunctionSymbol(name)

  // Boolean operators
  val And = new SqlOperator("and")
  val Or = new SqlOperator("or")
  val Not = new SqlOperator("not")

  // Numeric operators
  val + = new SqlOperator("+")
  val - = new SqlOperator("-")
  val * = new SqlOperator("*")
  val / = new SqlOperator("/")
  val Between = new FunctionSymbol("Between")

  // Comparison
  val < = new SqlOperator("<")
  val <= = new SqlOperator("<=")
  val > = new SqlOperator(">")
  val >= = new SqlOperator(">=")
  val == = new SqlOperator("=")

  // Set membership
  val In = new SqlOperator("in")

  // String functions
  val UCase = new JdbcFunction("ucase")
  val LCase = new JdbcFunction("lcase")
  val Like = new FunctionSymbol("Like")
  val StartsWith = new FunctionSymbol("StartsWith")
  val EndsWith = new FunctionSymbol("EndsWith")

  val CountAll = new FunctionSymbol("CountAll")
  val CountDistinct = new FunctionSymbol("CountDistinct")
  val Cast = new FunctionSymbol("Cast")
}

/** A Symbol that represents a library function or operator */
class FunctionSymbol(val name: String) extends Symbol {

  /** Create an untyped Apply of this Symbol */
  def apply(ch: Node*): Apply = Apply(this, ch)

  /** Match an Apply of this Symbol */
  def unapplySeq(n: Node) = n match {
    case Apply(sym, ch) if sym eq this => Some(ch)
    case _ => None
  }

  /** Create a typed Apply of this Symbol */
  def typed(tpe: Type, ch: Node*): Apply with TypedNode = Apply(this, ch, tpe)

  /** Create a typed Apply of this Symbol */
  def typed[T : TypeMapper](ch: Node*): Apply with TypedNode = Apply(this, ch, implicitly[TypeMapper[T]])

  /** Create a Column with a typed Apply of this Symbol */
  def column[T : TypeMapper](ch: Node*): OperatorColumn[T] = new OperatorColumn[T] {
    val nodeDelegate = typed[T](ch: _*)
  }

  override def toString = "Function "+name
}

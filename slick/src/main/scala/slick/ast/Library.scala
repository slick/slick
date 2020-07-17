package slick.ast

import slick.util.ConstArray

/**
 * The standard library for query operators.
 */
object Library {
  trait AggregateFunctionSymbol extends TermSymbol
  class JdbcFunction(name: String) extends FunctionSymbol(name) {
    override def hashCode = name.hashCode
    override def equals(o: Any) = o match {
      case o: JdbcFunction => name == o.name
      case _ => false
    }
  }
  class SqlFunction(name: String) extends FunctionSymbol(name)
  class SqlOperator(name: String) extends FunctionSymbol(name)
  class AggregateFunction(name: String) extends FunctionSymbol(name) with AggregateFunctionSymbol
  class SqlAggregateFunction(name: String) extends SqlFunction(name) with AggregateFunctionSymbol

  // Boolean operators
  val And = new SqlOperator("and")
  val Or = new SqlOperator("or")
  val Not = new SqlOperator("not")

  // Numeric operators and functions
  val + = new SqlOperator("+")
  val - = new SqlOperator("-")
  val * = new SqlOperator("*")
  val / = new SqlOperator("/")
  val % = new JdbcFunction("mod")
  val Between = new FunctionSymbol("between")
  val Abs = new JdbcFunction("abs")
  val Ceiling = new JdbcFunction("ceiling")
  val Floor = new JdbcFunction("floor")
  val Sign = new JdbcFunction("sign")
  val Degrees = new JdbcFunction("degrees")
  val Radians = new JdbcFunction("radians")

  // Comparison
  val < = new SqlOperator("<")
  val <= = new SqlOperator("<=")
  val > = new SqlOperator(">")
  val >= = new SqlOperator(">=")
  val == = new SqlOperator("=")
  val equal = == //TODO Dotty doesn't see == as a stable symbol

  // Set membership
  val In = new SqlOperator("in")

  // String functions
  val Length = new JdbcFunction("length")
  val Concat = new JdbcFunction("concat")
  val UCase = new JdbcFunction("ucase")
  val LCase = new JdbcFunction("lcase")
  val LTrim = new JdbcFunction("ltrim")
  val RTrim = new JdbcFunction("rtrim")
  val Replace = new JdbcFunction("replace")
  val Reverse = new SqlFunction("reverse")
  val Substring = new FunctionSymbol("substring")
  val Trim = new FunctionSymbol("Trim")
  val IndexOf = new FunctionSymbol("IndexOf")
  val Like = new FunctionSymbol("Like")
  val StartsWith = new FunctionSymbol("StartsWith")
  val EndsWith = new FunctionSymbol("EndsWith")
  val Repeat = new SqlFunction("repeat")

  // Aggregate functions
  val Min = new SqlAggregateFunction("min")
  val Max = new SqlAggregateFunction("max")
  val Avg = new SqlAggregateFunction("avg")
  val Sum = new SqlAggregateFunction("sum")
  val Count = new SqlAggregateFunction("count")
  val CountAll = new AggregateFunction("count(*)")
  val CountDistinct = new AggregateFunction("count distinct")

  val Exists = new SqlFunction("exists")

  /** A standard cast operation which usually requires code to be generated */
  val Cast = new FunctionSymbol("Cast")

  /** A type assignment describing an inherent type change that does not require any code to be
    * generated. It is used in SQL-like ASTs for assigning the proper scalar type to aggregating
    * subqueries which are used in a scalar context. */
  val SilentCast = new FunctionSymbol("SilentCast")

  val IfNull = new JdbcFunction("ifnull")

  // Values
  val User = new JdbcFunction("user")
  val Database = new JdbcFunction("database")
  val CurrentDate = new JdbcFunction("curdate")
  val CurrentTime = new JdbcFunction("curtime")
  val Pi = new JdbcFunction("pi")

  // Sequence operations
  val NextValue = new FunctionSymbol("NextValue")
  val CurrentValue = new FunctionSymbol("CurrentValue")
}

/** A Symbol that represents a library function or operator */
class FunctionSymbol(val name: String) extends TermSymbol {

  /** Match an Apply of this Symbol */
  def unapplySeq(a: Apply) = if(a.sym eq this) Some(a.children.toSeq) else None

  /** Create a typed Apply of this Symbol */
  def typed(tpe: Type, ch: Node*): Apply = Apply(this, ConstArray.from(ch))(tpe)

  /** Create a typed Apply of this Symbol */
  def typed[T : ScalaBaseType](ch: Node*): Apply = Apply(this, ConstArray.from(ch))(implicitly[ScalaBaseType[T]])

  override def toString = "Function "+name
}

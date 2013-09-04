package scala.slick
package lifted

/**
 * Aliases for lifted embedding features. This trait can be mixed into aliasing objects
 * which simplify the use of the lifted embedding.
 */
trait Aliases {
  type Query[+E, U] = lifted.Query[E, U]
  val Query = lifted.Query
  type TableQuery[+E <: AbstractTable[_], U] = lifted.TableQuery[E, U]
  val TableQuery = lifted.TableQuery
  type Column[T] = lifted.Column[T]
  type ConstColumn[T] = lifted.ConstColumn[T]
  val ConstColumn = lifted.ConstColumn
  type LiteralColumn[T] = lifted.LiteralColumn[T]
  val LiteralColumn = lifted.LiteralColumn
  val Case = lifted.Case
  type Rep[T] = lifted.Rep[T]
  val Functions = lifted.Functions
  type Parameters[PU, PP] = lifted.Parameters[PU, PP]
  val Parameters = lifted.Parameters
  type SimpleFunction = lifted.SimpleFunction
  val SimpleFunction = lifted.SimpleFunction
  type SimpleBinaryOperator = lifted.SimpleBinaryOperator
  val SimpleBinaryOperator = lifted.SimpleBinaryOperator
  type SimpleExpression = lifted.SimpleExpression
  val SimpleExpression = lifted.SimpleExpression
  type SimpleLiteral = lifted.SimpleLiteral
  val SimpleLiteral = lifted.SimpleLiteral
  val TupleMethods = util.TupleMethods
  type Tag = lifted.Tag
}

package scala.slick
package lifted

/**
 * Aliases for QL features. This trait can be mixed into aliasing objects
 * which simplify the use of the Query Language.
 */
trait Aliases {
  type Query[+E, U] = lifted.Query[E, U]
  val Query = lifted.Query
  type Column[T] = lifted.Column[T]
  type ConstColumn[T] = lifted.ConstColumn[T]
  val ConstColumn = lifted.ConstColumn
  val Case = lifted.Case
  type Re[T] = lifted.Rep[T]
  val Functions = lifted.Functions
  type Parameters[PU, PP] = lifted.Parameters[PU, PP]
  val Parameters = lifted.Parameters
  type Sequence[T] = lifted.Sequence[T]
  val Sequence = lifted.Sequence
  type SimpleFunction = lifted.SimpleFunction
  val SimpleFunction = lifted.SimpleFunction
  type SimpleBinaryOperator = lifted.SimpleBinaryOperator
  val SimpleBinaryOperator = lifted.SimpleBinaryOperator
  type SimpleExpression = lifted.SimpleExpression
  val SimpleExpression = lifted.SimpleExpression
  type SimpleLiteral = lifted.SimpleLiteral
  val SimpleLiteral = lifted.SimpleLiteral
  type TypeMapper[T] = lifted.TypeMapper[T]
  val TypeMapper = lifted.TypeMapper
  type TypeMapperDelegate[T] = lifted.TypeMapperDelegate[T]
  val TypeMapperDelegate = lifted.TypeMapperDelegate
  type MappedTypeMapper[T, U] = lifted.MappedTypeMapper[T, U]
  val MappedTypeMapper = lifted.MappedTypeMapper
  val ~ = lifted.~
}

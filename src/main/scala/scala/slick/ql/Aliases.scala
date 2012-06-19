package scala.slick
package ql

/**
 * Aliases for QL features. This trait can be mixed into aliasing objects
 * which simplify the use of the Query Language.
 */
trait Aliases {
  type Query[+E, U] = ql.Query[E, U]
  val Query = ql.Query
  type Column[T] = ql.Column[T]
  type ConstColumn[T] = ql.ConstColumn[T]
  val ConstColumn = ql.ConstColumn
  val Case = ql.Case
  type Re[T] = ql.Rep[T]
  val Functions = ql.Functions
  type Parameters[PU, PP] = ql.Parameters[PU, PP]
  val Parameters = ql.Parameters
  type Sequence[T] = ql.Sequence[T]
  val Sequence = ql.Sequence
  type SimpleFunction = ql.SimpleFunction
  val SimpleFunction = ql.SimpleFunction
  type SimpleBinaryOperator = ql.SimpleBinaryOperator
  val SimpleBinaryOperator = ql.SimpleBinaryOperator
  type SimpleExpression = ql.SimpleExpression
  val SimpleExpression = ql.SimpleExpression
  type SimpleLiteral = ql.SimpleLiteral
  val SimpleLiteral = ql.SimpleLiteral
  type TypeMapper[T] = ql.TypeMapper[T]
  val TypeMapper = ql.TypeMapper
  type TypeMapperDelegate[T] = ql.TypeMapperDelegate[T]
  val TypeMapperDelegate = ql.TypeMapperDelegate
  type MappedTypeMapper[T, U] = ql.MappedTypeMapper[T, U]
  val MappedTypeMapper = ql.MappedTypeMapper
  val ~ = ql.~
}

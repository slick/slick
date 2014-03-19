package scala.slick
package lifted

import scala.language.higherKinds

/**
 * Aliases for lifted embedding features. This trait can be mixed into aliasing objects
 * which simplify the use of the lifted embedding.
 */
trait Aliases {
  type Query[+E, U, C[_]] = lifted.Query[E, U, C]
  val Query = lifted.Query
  type TableQuery[E <: AbstractTable[_]] = lifted.TableQuery[E]
  val TableQuery = lifted.TableQuery
  type Compiled[T] = lifted.Compiled[T]
  val Compiled = lifted.Compiled
  type Column[T] = lifted.Column[T]
  type ConstColumn[T] = lifted.ConstColumn[T]
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
  type Shape[Level <: ShapeLevel, -M, U, P] = lifted.Shape[Level, M, U, P]
  type MappedProductShape[Level <: ShapeLevel, C, M <: C, U <: C, P <: C] = lifted.MappedProductShape[Level, C, M, U, P]
  type MappedScalaProductShape[Level <: ShapeLevel, C <: Product, M <: C, U <: C, P <: C] = lifted.MappedScalaProductShape[Level, C, M, U, P]
  type ShapeLevel = lifted.ShapeLevel
  val ShapeLevel = lifted.ShapeLevel
  type Isomorphism[A, B] = lifted.Isomorphism[A, B]
  type MappedTo[T] = lifted.MappedTo[T]
}

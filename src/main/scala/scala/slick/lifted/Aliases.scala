package scala.slick
package lifted

import scala.language.higherKinds

/** Aliases for lifted embedding features. This trait can be mixed into aliasing
  * objects which simplify the use of the lifted embedding. */
trait Aliases {
  type Query[+E, U, C[_]] = lifted.Query[E, U, C]
  val Query = lifted.Query
  type TableQuery[E <: AbstractTable[_]] = lifted.TableQuery[E]
  val TableQuery = lifted.TableQuery
  type Compiled[T] = lifted.Compiled[T]
  val Compiled = lifted.Compiled
  type ConstColumn[T] = lifted.ConstColumn[T]
  type LiteralColumn[T] = lifted.LiteralColumn[T]
  val LiteralColumn = lifted.LiteralColumn
  val Case = lifted.Case
  type Rep[T] = lifted.Rep[T]
  val Rep = lifted.Rep
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
  type CaseClassShape[P <: Product, LiftedTuple, LiftedCaseClass <: P, PlainTuple, PlainCaseClass <: P] = lifted.CaseClassShape[P, LiftedTuple, LiftedCaseClass, PlainTuple, PlainCaseClass]
  type ShapeLevel = lifted.ShapeLevel
  type NestedShapeLevel = lifted.NestedShapeLevel
  type FlatShapeLevel = lifted.FlatShapeLevel
  type ColumnsShapeLevel = lifted.ColumnsShapeLevel
  type Isomorphism[A, B] = lifted.Isomorphism[A, B]
  type MappedTo[T] = lifted.MappedTo[T]
  val ForeignKeyAction = scala.slick.model.ForeignKeyAction
  type ForeignKeyAction = scala.slick.model.ForeignKeyAction

  @deprecated("Use Rep[T : TypedType] instead of Column[T]", "2.2")
  type Column[T] = lifted.Rep[T]
  @deprecated("Use Rep[T : TypedType] instead of Column[T]", "2.2")
  val Column = lifted.Rep
}

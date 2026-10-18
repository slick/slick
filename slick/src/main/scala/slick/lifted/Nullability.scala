package slick.lifted

import scala.language.implicitConversions

import slick.ast.{BaseTypedType, OptionTypedType, TypedType}

/** Whether a value can be `NULL`, as a static marker in the `Nullability` type member of a [[Rep.TypedRep]].
  *
  * A `Nullable` value of type `Option[T]` gets the `Option` column operators, for which comparisons yield
  * `Option[Boolean]`. A `NotNull` value gets the plain ones, even if its type is an `Option` that a column type maps
  * as a whole to a non-null database value. `Nullability` itself marks a column whose nullability is not known; it
  * gets the operators that the `TypedType` instances in implicit scope select. */
sealed trait Nullability

object Nullability {
  sealed trait Nullable extends Nullability
  sealed trait NotNull extends Nullability
}

/** The `TypedType` of a column of Scala type `C`, and whether the column is nullable.
  *
  * It is found from the `TypedType` instances in implicit scope. A `BaseTypedType[C]` makes a non-nullable column, and
  * takes priority even when `C` is an `Option` type. Otherwise a `TypedType[T]` for `C = Option[T]` makes a nullable
  * column. Any other `TypedType[C]` makes a column of unknown nullability. A `TypedType` passed explicitly converts to
  * the same `ColumnTypeOf` that its static type would be found as. */
sealed trait ColumnTypeOf[C] {
  type Nullability <: slick.lifted.Nullability
  def tpe: TypedType[C]
}

object ColumnTypeOf extends ColumnTypeOfLowPriority {
  type Aux[C, N <: Nullability] = ColumnTypeOf[C] { type Nullability = N }

  private[lifted] def apply[C, N <: Nullability](t: TypedType[C]): Aux[C, N] = new ColumnTypeOf[C] {
    type Nullability = N
    def tpe = t
  }

  implicit def notNull[C](implicit t: BaseTypedType[C]): Aux[C, Nullability.NotNull] = apply(t)

  implicit def fromBaseTypedType[C](t: BaseTypedType[C]): Aux[C, Nullability.NotNull] = apply(t)
  implicit def fromOptionTypedType[T](t: OptionTypedType[T]): Aux[Option[T], Nullability.Nullable] = apply(t)
  implicit def fromTypedType[C](t: TypedType[C]): Aux[C, Nullability] = apply(t)
}

sealed trait ColumnTypeOfLowPriority extends ColumnTypeOfLowestPriority {
  implicit def nullable[T](implicit t: TypedType[T]): ColumnTypeOf.Aux[Option[T], Nullability.Nullable] =
    ColumnTypeOf(t.optionType)
}

sealed trait ColumnTypeOfLowestPriority {
  implicit def unknownNullability[C](implicit t: TypedType[C]): ColumnTypeOf.Aux[C, Nullability] = ColumnTypeOf(t)
}

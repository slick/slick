package slick.lifted
import slick.SlickException
import slick.ast.{FieldSymbol, OptionApply, Select, TermSymbol}
import slick.jdbc.{JdbcType, SpecializedJdbcResultConverter}

import java.sql.PreparedStatement
import scala.collection.immutable.ListMap

/**
  * A builder for table fields merging.
  */
class MergeAction[+E](shape: E) { update =>

  import slick.lifted.MergeAction._

  private var fields: Map[TermSymbol, FieldSetter] = ListMap.empty
  private var whens = Set.empty[AnyRef]

  /**
    * Create a chain for field merge action
    */
  def apply[F](f: E => RichRep[F]): RichField[F] = {
    new RichField[F](f(shape))
  }

  /**
    * Determine if a merge action exists for the [[field]]
    */
  def isDirty(field: TermSymbol) = {
    fields.contains(field)
  }

  /**
    * Set parameter values to the underlying [[PreparedStatement]] for fields that should update
    */
  def prepare(st: PreparedStatement) = {
    checkUpdates()
    fields.valuesIterator.foreach(_(st))
  }

  //XXX: macros may be introduced for more detailed diagnostic info
  private def checkUpdates() = {
    val missing = whens.size
    if (missing > 0) throw new SlickException(s"${missing} value${if (missing > 1) "s are" else "is"} missing for when()")
  }


  /**
    * A chain to express a merge action for a field
    */
  class RichField[F] private [MergeAction](richRep: RichRep[F]) {

    /**
      * Update the value for this field
      * @param value The new value
      */
    def set(value: F) = {
      fields += richRep.symbolToSetter(Some(value), fields.size)
      update
    }

    /**
      * Set this field to [[NULL]]
      */
    def remove() = {
      fields += richRep.symbolToSetter(None, fields.size)
      update
    }

    /**
      * Update the value for this field, if [[value]] is [[Some]]
      *
      * @param value The new value
      */
    def option(value: Option[F]) = {
      value.foreach { v =>
        fields += richRep.symbolToSetter(Some(v), fields.size)
      }
      update
    }

    /**
      * Update the value for this field, when [[predicate]] is [[true]]
      *
      * @see [[When]]
      */
    def when(predicate: Boolean) = {
      val when = new When(predicate)
      whens += when
      when
    }

    /**
      * A holder for fields update with predicates
      * @param predicate update should be applied if [[true]], no action for [[false]]
      */
    class When private [RichField] (predicate: Boolean) {

      /**
        * Update this field with supplied [[value]] if [[predicate]] is [[true]]
        */
      def set(value: F) = {
        if (predicate) fields += richRep.symbolToSetter(Some(value), fields.size)
        whens -= this
        update
      }

      /**
        * Update this field with supplied `value.get()` if [[predicate]] is [[true]] and [[value]] is [[Some]]
        */
      def option(value: Option[F]) = {
        value.foreach { v =>
          fields += richRep.symbolToSetter(Some(v), fields.size)
        }
        whens -= this
        update
      }

      /**
        * Set this field to `NULL` if [[predicate]] is [[true]]
        */
      def remove() = {
        fields += richRep.symbolToSetter(None, fields.size)
        update
      }
    }
  }

}

object MergeAction {

  type FieldSetter = PreparedStatement => Unit
  type FieldTuple = (TermSymbol, FieldSetter)

  trait RichRep[T] {
    type Original <: Rep[_]
    val isOption: Boolean
    val original: Original
    def symbolToSetter(value: Option[T], index: Int): FieldTuple
  }

  implicit class PureRichRep[T](override val original: Rep[T]) extends RichRep[T] {
    override type Original = Rep[T]
    override val isOption: Boolean = false

    override def symbolToSetter(value: Option[T], index: Int): FieldTuple = {
      val Select(_, field) = original.toNode
      val setter: FieldSetter =
        field match {
          case f: FieldSymbol =>
            value match {
              case Some(v) =>
                pp => SpecializedJdbcResultConverter.base(f.tpe.asInstanceOf[JdbcType[T]], f.name, index).set(v, pp, 1)
              case None =>
                pp => pp.setNull(index + 1, f.tpe.asInstanceOf[JdbcType[T]].sqlType)
            }

          case _ => throw new SlickException(s"Unsupported field: $original")
        }
      (field, setter)
    }

  }

  implicit class OptionRichRep[T](override val original: Rep[Option[T]]) extends RichRep[Option[T]] {

    override type Original = Rep[Option[T]]
    override val isOption: Boolean = true

    override def symbolToSetter(value: Option[Option[T]], index: Int): FieldTuple = {
      val OptionApply(Select(_, field)) = original.toNode

      val setter: FieldSetter = field match {
        case f: FieldSymbol =>
          value match {
            case Some(v) =>
              pp => SpecializedJdbcResultConverter.option(f.tpe.asInstanceOf[JdbcType[T]], index).set(v, pp, 1)
            case None =>
              pp => pp.setNull(index + 1, f.tpe.asInstanceOf[JdbcType[T]].sqlType)
          }

        case _ => throw new SlickException(s"Unsupported field: $original")
      }

      (field, setter)
    }

  }

}

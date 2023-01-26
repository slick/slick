package slick.jdbc

import java.sql.PreparedStatement

import scala.collection.immutable.ListMap

import slick.SlickException
import slick.ast._
import slick.lifted.Rep

import slick.jdbc.JdbcMergeBuilder._

/**
  * A builder for table fields merging.
  */
class JdbcMergeBuilder[+E](shape: E, factory: FieldSetterFactory) { update =>

  private[this] var fields: Map[FieldSymbol, FieldSetter] = ListMap.empty
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
  def isDirty(field: FieldSymbol) = {
    fields.contains(field)
  }

  def isEmpty() = {
    fields.isEmpty
  }

  /**
    * Set parameter values to the underlying for fields that should update
    */
  def fieldSetters = {
    checkUpdates()
    fields
  }

  //XXX: macros may be introduced for more detailed diagnostic info
  private def checkUpdates() = {
    val missing = whens.size
    if (missing > 0) throw new SlickException(s"${missing} value${if (missing > 1) "s are" else " is"} missing for when()")
  }

  private def mergeField(tuple: FieldTuple) = {
    if (fields.contains(tuple._1)) {
      throw new SlickException(s"Multiple merge actions on the same field is not supported, field: ${tuple._1}")
    }
    fields += tuple
  }


  /**
    * A chain to express a merge action for a field
    * F == Option[Int]
    */
  class RichField[F] private [JdbcMergeBuilder](richRep: RichRep[F]) {

    /**
      * Update the value for this field
      * @param value The new value
      */
    def set(value: F) = {
      mergeField(richRep.symbolToSetter(factory)(Some(value)))
      update
    }

    /**
      * Set this field to [[NULL]]
      */
    def remove() = {
      mergeField(richRep.symbolToSetter(factory)(None))
      update
    }

    /**
      * Update the value for this field, if [[value]] is [[Some]]
      *
      * @param value The new value
      */
    def option(value: Option[F]) = {
      value.foreach { v =>
        mergeField(richRep.symbolToSetter(factory)(Some(v)))
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
        if (predicate) mergeField(richRep.symbolToSetter(factory)(Some(value)))
        whens -= this
        update
      }

      /**
        * Update this field with supplied `value.get()` if [[predicate]] is [[true]] and [[value]] is [[Some]]
        */
      def option(value: Option[F]) = {
        if (predicate) value.foreach { v =>
          mergeField(richRep.symbolToSetter(factory)(Some(v)))
        }
        whens -= this
        update
      }

      /**
        * Set this field to `NULL` if [[predicate]] is [[true]]
        */
      def remove() = {
        if (predicate) mergeField(richRep.symbolToSetter(factory)(None))
        whens -= this
        update
      }
    }
  }

}

object JdbcMergeBuilder {

  type FieldSetter = (PreparedStatement, Int) => Unit
  type FieldTuple = (FieldSymbol, FieldSetter)

  trait FieldSetterFactory {
    def apply[T](field: FieldSymbol, tpe: TypedType[T], value: Option[T]): FieldSetter
  }

  trait RichRep[T] {
    type Original <: Rep[_]
    val isOption: Boolean
    val original: Original
    def symbolToSetter(factory: FieldSetterFactory)(value: Option[T]): FieldTuple
  }

  implicit class PureRichRep[T](override val original: Rep[T]) extends RichRep[T] {
    override type Original = Rep[T]
    override val isOption: Boolean = false

    override def symbolToSetter(factory: FieldSetterFactory)(value: Option[T]): FieldTuple = {
      val field = extractField(original.toNode)
      field match {
        case f: FieldSymbol =>
          val tpe: TypedType[T] = extractType(original)
          (f, factory(f, tpe, value))
        case _ => throw new SlickException(s"Unsupported field: $original")
      }
    }

  }

  implicit class OptionRichRep[T](override val original: Rep[Option[T]]) extends RichRep[Option[T]] {

    override type Original = Rep[Option[T]]
    override val isOption: Boolean = true

    override def symbolToSetter(factory: FieldSetterFactory)(value: Option[Option[T]]): FieldTuple = {
      val field = extractField(original.toNode)
      val pure = value match {
        case Some(v) => v
        case None => None
      }

      field match {
        case f: FieldSymbol =>
          val tpe: TypedType[T] = extractType(original)
          (f, factory(f, tpe, pure))

        case _ => throw new SlickException(s"Unsupported field: $original")
      }
    }

  }

  private def extractField(node: Node) = {
    node match {
      case OptionApply(Select(_, field)) => field
      case Select(_, field) => field
      case _ => throw new SlickException(s"Unsupported field: $node")
    }
  }

  private def extractType[T](rep: Rep[_]): TypedType[T] = {
    try {
      rep.asInstanceOf[Rep.TypedRep[T]].tpe match {
        case x: OptionTypedType[_] => x.elementType.asInstanceOf[TypedType[T]]
        case x => x
      }
    } catch {
      case ex: ClassCastException => throw new SlickException(s"Unsupported column: ${rep}", ex)
    }
  }

}

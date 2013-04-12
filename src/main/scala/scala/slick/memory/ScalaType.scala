package scala.slick.memory

import scala.slick.ast.{OptionTypedType, NumericTypedType, BaseTypedType, Ordering, TypedType}
import scala.reflect.ClassTag

/** A Slick Type encoding of plain Scala types.
  *
  * This is used by QueryInterpreter and MemoryDriver. Values stored in
  * HeapBackend columns are also expected to use these types.
  */
trait ScalaType[T] extends TypedType[T] {
  override def optionType: ScalaOptionType[T] = new ScalaOptionType[T](this)
  def nullable: Boolean
  def zero: T
  def scalaOrderingFor(ord: Ordering): scala.math.Ordering[T]
}

class ScalaBaseType[T](val zero: T)(implicit val tag: ClassTag[T], val ordering: scala.math.Ordering[T]) extends ScalaType[T] with BaseTypedType[T] {
  override def toString = "ScalaType[" + tag.runtimeClass.getName + "]"
  def nullable = false
  def scalaOrderingFor(ord: Ordering) = {
    val base = if(ord.direction == Ordering.Desc) ordering.reverse else ordering
    val nullsFirst = if(ord.nulls == Ordering.NullsFirst) -1 else 1
    new scala.math.Ordering[T] {
      def compare(x: T, y: T): Int = {
        if((x.asInstanceOf[AnyRef] eq null) && (y.asInstanceOf[AnyRef] eq null)) 0
        else if(x.asInstanceOf[AnyRef] eq null) nullsFirst
        else if(y.asInstanceOf[AnyRef] eq null) -nullsFirst
        else base.compare(x, y)
      }
    }
  }
}

class ScalaNumericType[T](implicit tag: ClassTag[T], val numeric: Numeric[T])
  extends ScalaBaseType[T](numeric.zero)(tag, numeric) with NumericTypedType

class ScalaOptionType[T](val elementType: ScalaType[T]) extends ScalaType[Option[T]] with OptionTypedType[T] {
  override def toString = "ScalaOptionType[" + elementType + "]"
  def nullable = true
  def zero = None
  def scalaOrderingFor(ord: Ordering) = {
    val nullsFirst = if(ord.nulls == Ordering.NullsFirst) -1 else 1
    new scala.math.Ordering[Option[T]] {
      def compare(x: Option[T], y: Option[T]): Int = {
        if(x == None && y == None) 0
        else if(x == None) nullsFirst
        else if(y == None) -nullsFirst
        else elementType.scalaOrderingFor(ord).compare(x.get, y.get)
      }
    }
  }
}

object ScalaType {
  val booleanType = new ScalaBaseType[Boolean](false)
  val bigDecimalType = new ScalaNumericType[BigDecimal]
  val byteType = new ScalaNumericType[Byte]
  val charType = new ScalaBaseType[Char](' ')
  val doubleType = new ScalaNumericType[Double]
  val floatType = new ScalaNumericType[Float]
  val intType = new ScalaNumericType[Int]
  val longType = new ScalaNumericType[Long]
  val nullType = new ScalaBaseType[Null](null)
  val shortType = new ScalaNumericType[Short]
  val stringType = new ScalaBaseType[String]("")
  val unitType = new ScalaBaseType[Unit](())
}

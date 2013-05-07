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

abstract class ScalaNumericType[T](implicit tag: ClassTag[T], val numeric: Numeric[T])
  extends ScalaBaseType[T](numeric.zero)(tag, numeric) with NumericTypedType {
  def fromDouble(v: Double): T
  def toDouble(v: T) = numeric.toDouble(v)
}

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
  val bigDecimalType: ScalaNumericType[BigDecimal] = new ScalaNumericType[BigDecimal] {
    def fromDouble(v: Double) = BigDecimal(v)
  }
  val byteType: ScalaNumericType[Byte] = new ScalaNumericType[Byte] {
    def fromDouble(v: Double) = v.toByte
  }
  val charType = new ScalaBaseType[Char](' ')
  val doubleType: ScalaNumericType[Double] = new ScalaNumericType[Double] {
    def fromDouble(v: Double) = v
  }
  val floatType: ScalaNumericType[Float] = new ScalaNumericType[Float] {
    def fromDouble(v: Double) = v.toFloat
  }
  val intType: ScalaNumericType[Int] = new ScalaNumericType[Int] {
    def fromDouble(v: Double) = v.toInt
  }
  val longType: ScalaNumericType[Long] = new ScalaNumericType[Long] {
    def fromDouble(v: Double) = v.toLong
  }
  val nullType = new ScalaBaseType[Null](null)
  val shortType: ScalaNumericType[Short] = new ScalaNumericType[Short] {
    def fromDouble(v: Double) = v.toShort
  }
  val stringType = new ScalaBaseType[String]("")
  val unitType = new ScalaBaseType[Unit](())
}

package org.scalaquery.simple

import java.sql.{SQLException, Date, Time, Timestamp}
import org.scalaquery.session.PositionedParameters

/**
 * Basic conversions for setting parameters in PositionedParameters.
 */
trait SetParameter[-T] extends ((T, PositionedParameters) => Unit)

object SetParameter {
  implicit object SetBoolean extends SetParameter[Boolean] { def apply(v: Boolean, pp: PositionedParameters) { pp.setBoolean(v) } }
  implicit object SetByte extends SetParameter[Byte] { def apply(v: Byte, pp: PositionedParameters) { pp.setByte(v) } }
  implicit object SetDate extends SetParameter[Date] { def apply(v: Date, pp: PositionedParameters) { pp.setDate(v) } }
  implicit object SetDouble extends SetParameter[Double] { def apply(v: Double, pp: PositionedParameters) { pp.setDouble(v) } }
  implicit object SetFloat extends SetParameter[Float] { def apply(v: Float, pp: PositionedParameters) { pp.setFloat(v) } }
  implicit object SetInt extends SetParameter[Int] { def apply(v: Int, pp: PositionedParameters) { pp.setInt(v) } }
  implicit object SetLong extends SetParameter[Long] { def apply(v: Long, pp: PositionedParameters) { pp.setLong(v) } }
  implicit object SetShort extends SetParameter[Short] { def apply(v: Short, pp: PositionedParameters) { pp.setShort(v) } }
  implicit object SetString extends SetParameter[String] { def apply(v: String, pp: PositionedParameters) { pp.setString(v) } }
  implicit object SetTime extends SetParameter[Time] { def apply(v: Time, pp: PositionedParameters) { pp.setTime(v) } }
  implicit object SetTimestamp extends SetParameter[Timestamp] { def apply(v: Timestamp, pp: PositionedParameters) { pp.setTimestamp(v) } }

  implicit object SetBooleanOption extends SetParameter[Option[Boolean]] { def apply(v: Option[Boolean], pp: PositionedParameters) { pp.setBooleanOption(v) } }
  implicit object SetByteOption extends SetParameter[Option[Byte]] { def apply(v: Option[Byte], pp: PositionedParameters) { pp.setByteOption(v) } }
  implicit object SetDateOption extends SetParameter[Option[Date]] { def apply(v: Option[Date], pp: PositionedParameters) { pp.setDateOption(v) } }
  implicit object SetDoubleOption extends SetParameter[Option[Double]] { def apply(v: Option[Double], pp: PositionedParameters) { pp.setDoubleOption(v) } }
  implicit object SetFloatOption extends SetParameter[Option[Float]] { def apply(v: Option[Float], pp: PositionedParameters) { pp.setFloatOption(v) } }
  implicit object SetIntOption extends SetParameter[Option[Int]] { def apply(v: Option[Int], pp: PositionedParameters) { pp.setIntOption(v) } }
  implicit object SetLongOption extends SetParameter[Option[Long]] { def apply(v: Option[Long], pp: PositionedParameters) { pp.setLongOption(v) } }
  implicit object SetShortOption extends SetParameter[Option[Short]] { def apply(v: Option[Short], pp: PositionedParameters) { pp.setShortOption(v) } }
  implicit object SetStringOption extends SetParameter[Option[String]] { def apply(v: Option[String], pp: PositionedParameters) { pp.setStringOption(v) } }
  implicit object SetTimeOption extends SetParameter[Option[Time]] { def apply(v: Option[Time], pp: PositionedParameters) { pp.setTimeOption(v) } }
  implicit object SetTimestampOption extends SetParameter[Option[Timestamp]] { def apply(v: Option[Timestamp], pp: PositionedParameters) { pp.setTimestampOption(v) } }

  implicit object SetProduct extends SetParameter[Product] {
    def apply(prod: Product, pp: PositionedParameters): Unit =
      for(i <- 0 until prod.productArity) prod.productElement(i) match {
        case v: Boolean => pp.setBoolean(v)
        case v: Byte => pp.setByte(v)
        case v: Date => pp.setDate(v)
        case v: Double => pp.setDouble(v)
        case v: Float => pp.setFloat(v)
        case v: Int => pp.setInt(v)
        case v: Long => pp.setLong(v)
        case v: Short => pp.setShort(v)
        case v: String => pp.setString(v)
        case v: Time => pp.setTime(v)
        case v: Timestamp => pp.setTimestamp(v)
        case v => throw new SQLException("SetProduct doesn't know how to handle parameter "+i+"( "+v+")")
      }
  }

  implicit object SetUnit extends SetParameter[Unit] { def apply(none: Unit, pp: PositionedParameters) = () }

  def apply[T](implicit f: (T, PositionedParameters) => Unit) = new SetParameter[T] {
    def apply(v: T, pp: PositionedParameters) = f(v, pp)
  }
}

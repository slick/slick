package org.scalaquery.simple

import java.sql.{PreparedStatement, SQLException, Date, Time, Timestamp}

/**
 * Basic conversions for setting parameters in PreparedStatements.
 */
trait SetParameter[-T] extends ((T, PreparedStatement) => Unit)

object SetParameter {
  implicit object SetBoolean extends SetParameter[Boolean] { def apply(v: Boolean, st: PreparedStatement) { st.setBoolean(1, v) } }
  implicit object SetByte extends SetParameter[Byte] { def apply(v: Byte, st: PreparedStatement) { st.setByte(1, v) } }
  implicit object SetDate extends SetParameter[Date] { def apply(v: Date, st: PreparedStatement) { st.setDate(1, v) } }
  implicit object SetDouble extends SetParameter[Double] { def apply(v: Double, st: PreparedStatement) { st.setDouble(1, v) } }
  implicit object SetFloat extends SetParameter[Float] { def apply(v: Float, st: PreparedStatement) { st.setFloat(1, v) } }
  implicit object SetInt extends SetParameter[Int] { def apply(v: Int, st: PreparedStatement) { st.setInt(1, v) } }
  implicit object SetLong extends SetParameter[Long] { def apply(v: Long, st: PreparedStatement) { st.setLong(1, v) } }
  implicit object SetShort extends SetParameter[Short] { def apply(v: Short, st: PreparedStatement) { st.setShort(1, v) } }
  implicit object SetString extends SetParameter[String] { def apply(v: String, st: PreparedStatement) { st.setString(1, v) } }
  implicit object SetTime extends SetParameter[Time] { def apply(v: Time, st: PreparedStatement) { st.setTime(1, v) } }
  implicit object SetTimestamp extends SetParameter[Timestamp] { def apply(v: Timestamp, st: PreparedStatement) { st.setTimestamp(1, v) } }

  implicit object SetProduct extends SetParameter[Product] {
    def apply(prod: Product, st: PreparedStatement): Unit =
      for(i <- 0 until prod.productArity) prod.productElement(i) match {
        case v: Boolean => st.setBoolean(i+1, v)
        case v: Byte => st.setByte(i+1, v)
        case v: Date => st.setDate(i+1, v)
        case v: Double => st.setDouble(i+1, v)
        case v: Float => st.setFloat(i+1, v)
        case v: Int => st.setInt(i+1, v)
        case v: Long => st.setLong(i+1, v)
        case v: Short => st.setShort(i+1, v)
        case v: String => st.setString(i+1, v)
        case v: Time => st.setTime(i+1, v)
        case v: Timestamp => st.setTimestamp(i+1, v)
        case v => throw new SQLException("SetProduct doesn't know how to handle parameter "+i+"( "+v+")")
      }
  }

  implicit object SetUnit extends SetParameter[Unit] { def apply(none: Unit, st: PreparedStatement) = () }

  implicit def functionToSetParameter[T](implicit f: (T, PreparedStatement) => Unit) = new SetParameter[T] {
    def apply(v: T, st: PreparedStatement) = f(v, st)
  }
}

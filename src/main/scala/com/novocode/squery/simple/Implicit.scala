package com.novocode.squery.simple

import java.sql.{PreparedStatement, SQLException, Date, Time, Timestamp}
import com.novocode.squery.session.PositionedResult


/**
 * Implicit conversions for simple query parameters and query results
 */
object Implicit {

  // Implicit methods to convert results

  implicit def rsToBooleanOption(rs: PositionedResult) = rs.nextBooleanOption()
  implicit def rsToByteOption(rs: PositionedResult) = rs.nextByteOption()
  implicit def rsToDateOption(rs: PositionedResult) = rs.nextDateOption()
  implicit def rsToDoubleOption(rs: PositionedResult) = rs.nextDoubleOption()
  implicit def rsToFloatOption(rs: PositionedResult) = rs.nextFloatOption()
  implicit def rsToIntOption(rs: PositionedResult) = rs.nextIntOption()
  implicit def rsToLongOption(rs: PositionedResult) = rs.nextLongOption()
  implicit def rsToShortOption(rs: PositionedResult) = rs.nextShortOption()
  implicit def rsToStringOption(rs: PositionedResult) = rs.nextStringOption()
  implicit def rsToTimeOption(rs: PositionedResult) = rs.nextTimeOption()
  implicit def rsToTimestampOption(rs: PositionedResult) = rs.nextTimestampOption()

  implicit def rsToBoolean(rs: PositionedResult) = rs.nextBoolean()
  implicit def rsToByte(rs: PositionedResult) = rs.nextByte()
  implicit def rsToDate(rs: PositionedResult) = rs.nextDate()
  implicit def rsToDouble(rs: PositionedResult) = rs.nextDouble()
  implicit def rsToFloat(rs: PositionedResult) = rs.nextFloat()
  implicit def rsToInt(rs: PositionedResult) = rs.nextInt()
  implicit def rsToLong(rs: PositionedResult) = rs.nextLong()
  implicit def rsToShort(rs: PositionedResult) = rs.nextShort()
  implicit def rsToString(rs: PositionedResult) = rs.nextString()
  implicit def rsToTime(rs: PositionedResult) = rs.nextTime()
  implicit def rsToTimestamp(rs: PositionedResult) = rs.nextTimestamp()


  // Implicit methods to convert parameters

  implicit def prepareFromBoolean(v: Boolean, st: PreparedStatement) { st.setBoolean(1, v) }
  implicit def prepareFromByte(v: Byte, st: PreparedStatement) { st.setByte(1, v) }
  implicit def prepareFromDate(v: Date, st: PreparedStatement) { st.setDate(1, v) }
  implicit def prepareFromDouble(v: Double, st: PreparedStatement) { st.setDouble(1, v) }
  implicit def prepareFromFloat(v: Float, st: PreparedStatement) { st.setFloat(1, v) }
  implicit def prepareFromInt(v: Int, st: PreparedStatement) { st.setInt(1, v) }
  implicit def prepareFromLong(v: Long, st: PreparedStatement) { st.setLong(1, v) }
  implicit def prepareFromShort(v: Short, st: PreparedStatement) { st.setShort(1, v) }
  implicit def prepareFromString(v: String, st: PreparedStatement) { st.setString(1, v) }
  implicit def prepareFromTime(v: Time, st: PreparedStatement) { st.setTime(1, v) }
  implicit def prepareFromTimestamp(v: Timestamp, st: PreparedStatement) { st.setTimestamp(1, v) }

  implicit def prepareFromProduct(prod: Product, st: PreparedStatement): Unit =
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
      case v => throw new SQLException("prepareFromProduct doesn't know how to handle parameter "+i+"( "+v+")")
    }

  implicit def prepareFromUnit(none: Unit, st: PreparedStatement) = ()
}

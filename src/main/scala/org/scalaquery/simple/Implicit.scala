package org.scalaquery.simple

import java.sql.{PreparedStatement, SQLException, Date, Time, Timestamp}
import org.scalaquery.session.PositionedResult

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

  implicit def rsToTuple2[T1, T2](implicit c1: PositionedResult => T1, c2: PositionedResult => T2): (PositionedResult => (T1, T2)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs)))
  implicit def rsToTuple3[T1, T2, T3](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3): (PositionedResult => (T1, T2, T3)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs)))
  implicit def rsToTuple4[T1, T2, T3, T4](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4): (PositionedResult => (T1, T2, T3, T4)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs)))
  implicit def rsToTuple5[T1, T2, T3, T4, T5](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5): (PositionedResult => (T1, T2, T3, T4, T5)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs)))
  implicit def rsToTuple6[T1, T2, T3, T4, T5, T6](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6): (PositionedResult => (T1, T2, T3, T4, T5, T6)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs)))
  implicit def rsToTuple7[T1, T2, T3, T4, T5, T6, T7](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs)))
  implicit def rsToTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs)))
  implicit def rsToTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs)))
  implicit def rsToTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs)))
  implicit def rsToTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs)))
  implicit def rsToTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs)))
  implicit def rsToTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs)))
  implicit def rsToTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs)))
  implicit def rsToTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs)))
  implicit def rsToTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs)))
  implicit def rsToTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16, c17: PositionedResult => T17): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs)))
  implicit def rsToTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16, c17: PositionedResult => T17, c18: PositionedResult => T18): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs)))
  implicit def rsToTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16, c17: PositionedResult => T17, c18: PositionedResult => T18, c19: PositionedResult => T19): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs)))
  implicit def rsToTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16, c17: PositionedResult => T17, c18: PositionedResult => T18, c19: PositionedResult => T19, c20: PositionedResult => T20): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs), c20(rs)))
  implicit def rsToTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16, c17: PositionedResult => T17, c18: PositionedResult => T18, c19: PositionedResult => T19, c20: PositionedResult => T20, c21: PositionedResult => T21): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs), c20(rs), c21(rs)))
  implicit def rsToTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit c1: PositionedResult => T1, c2: PositionedResult => T2, c3: PositionedResult => T3, c4: PositionedResult => T4, c5: PositionedResult => T5, c6: PositionedResult => T6, c7: PositionedResult => T7, c8: PositionedResult => T8, c9: PositionedResult => T9, c10: PositionedResult => T10, c11: PositionedResult => T11, c12: PositionedResult => T12, c13: PositionedResult => T13, c14: PositionedResult => T14, c15: PositionedResult => T15, c16: PositionedResult => T16, c17: PositionedResult => T17, c18: PositionedResult => T18, c19: PositionedResult => T19, c20: PositionedResult => T20, c21: PositionedResult => T21, c22: PositionedResult => T22): (PositionedResult => (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)) =
    ((rs: PositionedResult) => (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs), c20(rs), c21(rs), c22(rs)))

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

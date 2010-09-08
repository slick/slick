package org.scalaquery.simple

import java.sql.{SQLException, Date, Time, Timestamp}
import org.scalaquery.SQueryException
import org.scalaquery.session.PositionedResult

/**
 * Basic conversions for extracting values from PositionedResults.
 */
trait GetResult[+T] extends (PositionedResult => T) { self =>
  override def andThen[A](g: T => A): GetResult[A] = new GetResult[A] { def apply(rs: PositionedResult): A = g(self.apply(rs)) }
}

object GetResult {
  implicit object GetBoolean extends GetResult[Boolean] { def apply(rs: PositionedResult) = rs.nextBoolean() }
  implicit object GetByte extends GetResult[Byte] { def apply(rs: PositionedResult) = rs.nextByte() }
  implicit object GetDate extends GetResult[Date] { def apply(rs: PositionedResult) = rs.nextDate() }
  implicit object GetDouble extends GetResult[Double] { def apply(rs: PositionedResult) = rs.nextDouble() }
  implicit object GetFloat extends GetResult[Float] { def apply(rs: PositionedResult) = rs.nextFloat() }
  implicit object GetInt extends GetResult[Int] { def apply(rs: PositionedResult) = rs.nextInt() }
  implicit object GetLong extends GetResult[Long] { def apply(rs: PositionedResult) = rs.nextLong() }
  implicit object GetShort extends GetResult[Short] { def apply(rs: PositionedResult) = rs.nextShort() }
  implicit object GetString extends GetResult[String] { def apply(rs: PositionedResult) = rs.nextString() }
  implicit object GetTime extends GetResult[Time] { def apply(rs: PositionedResult) = rs.nextTime() }
  implicit object GetTimestamp extends GetResult[Timestamp] { def apply(rs: PositionedResult) = rs.nextTimestamp() }

  implicit object GetBooleanOption extends GetResult[Option[Boolean]] { def apply(rs: PositionedResult) = rs.nextBooleanOption() }
  implicit object GetByteOption extends GetResult[Option[Byte]] { def apply(rs: PositionedResult) = rs.nextByteOption() }
  implicit object GetDateOption extends GetResult[Option[Date]] { def apply(rs: PositionedResult) = rs.nextDateOption() }
  implicit object GetDoubleOption extends GetResult[Option[Double]] { def apply(rs: PositionedResult) = rs.nextDoubleOption() }
  implicit object GetFloatOption extends GetResult[Option[Float]] { def apply(rs: PositionedResult) = rs.nextFloatOption() }
  implicit object GetIntOption extends GetResult[Option[Int]] { def apply(rs: PositionedResult) = rs.nextIntOption() }
  implicit object GetLongOption extends GetResult[Option[Long]] { def apply(rs: PositionedResult) = rs.nextLongOption() }
  implicit object GetShortOption extends GetResult[Option[Short]] { def apply(rs: PositionedResult) = rs.nextShortOption() }
  implicit object GetStringOption extends GetResult[Option[String]] { def apply(rs: PositionedResult) = rs.nextStringOption() }
  implicit object GetTimeOption extends GetResult[Option[Time]] { def apply(rs: PositionedResult) = rs.nextTimeOption() }
  implicit object GetTimestampOption extends GetResult[Option[Timestamp]] { def apply(rs: PositionedResult) = rs.nextTimestampOption() }

  implicit def createGetTuple2[T1, T2](implicit c1: GetResult[T1], c2: GetResult[T2]): GetResult[(T1, T2)] = new GetResult[(T1, T2)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs))
  }
  implicit def createGetTuple3[T1, T2, T3](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3]): GetResult[(T1, T2, T3)] = new GetResult[(T1, T2, T3)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs))
  }
  implicit def createGetTuple4[T1, T2, T3, T4](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4]): GetResult[(T1, T2, T3, T4)] = new GetResult[(T1, T2, T3, T4)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs))
  }
  implicit def createGetTuple5[T1, T2, T3, T4, T5](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5]): GetResult[(T1, T2, T3, T4, T5)] = new GetResult[(T1, T2, T3, T4, T5)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs))
  }
  implicit def createGetTuple6[T1, T2, T3, T4, T5, T6](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6]): GetResult[(T1, T2, T3, T4, T5, T6)] = new GetResult[(T1, T2, T3, T4, T5, T6)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs))
  }
  implicit def createGetTuple7[T1, T2, T3, T4, T5, T6, T7](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7]): GetResult[(T1, T2, T3, T4, T5, T6, T7)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs))
  }
  implicit def createGetTuple8[T1, T2, T3, T4, T5, T6, T7, T8](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs))
  }
  implicit def createGetTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs))
  }
  implicit def createGetTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs))
  }
  implicit def createGetTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs))
  }
  implicit def createGetTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs))
  }
  implicit def createGetTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs))
  }
  implicit def createGetTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs))
  }
  implicit def createGetTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs))
  }
  implicit def createGetTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs))
  }
  implicit def createGetTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16], c17: GetResult[T17]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs))
  }
  implicit def createGetTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16], c17: GetResult[T17], c18: GetResult[T18]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs))
  }
  implicit def createGetTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16], c17: GetResult[T17], c18: GetResult[T18], c19: GetResult[T19]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs))
  }
  implicit def createGetTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16], c17: GetResult[T17], c18: GetResult[T18], c19: GetResult[T19], c20: GetResult[T20]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs), c20(rs))
  }
  implicit def createGetTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16], c17: GetResult[T17], c18: GetResult[T18], c19: GetResult[T19], c20: GetResult[T20], c21: GetResult[T21]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs), c20(rs), c21(rs))
  }
  implicit def createGetTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit c1: GetResult[T1], c2: GetResult[T2], c3: GetResult[T3], c4: GetResult[T4], c5: GetResult[T5], c6: GetResult[T6], c7: GetResult[T7], c8: GetResult[T8], c9: GetResult[T9], c10: GetResult[T10], c11: GetResult[T11], c12: GetResult[T12], c13: GetResult[T13], c14: GetResult[T14], c15: GetResult[T15], c16: GetResult[T16], c17: GetResult[T17], c18: GetResult[T18], c19: GetResult[T19], c20: GetResult[T20], c21: GetResult[T21], c22: GetResult[T22]): GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] = new GetResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] {
    def apply(rs: PositionedResult) = (c1(rs), c2(rs), c3(rs), c4(rs), c5(rs), c6(rs), c7(rs), c8(rs), c9(rs), c10(rs), c11(rs), c12(rs), c13(rs), c14(rs), c15(rs), c16(rs), c17(rs), c18(rs), c19(rs), c20(rs), c21(rs), c22(rs))
  }

  private[simple] object GetUpdateValue extends GetResult[Int] {
    def apply(pr: PositionedResult) =
      throw new SQueryException("Update statements should not return a ResultSet")
  }

  def apply[T](implicit f: (PositionedResult => T)) = new GetResult[T] { def apply(rs: PositionedResult) = f(rs) }
}

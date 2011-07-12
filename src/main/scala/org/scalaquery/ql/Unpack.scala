package org.scalaquery.ql

import scala.annotation.implicitNotFound
import org.scalaquery.SQueryException

/**
 * A type class that encodes the unpacking `From => To` of a `Query[From]` to
 * its result element type `To`. At run-time, it contains the mapping to the
 * TypeMappers of the linearized type of the query.
 *
 * =Example:=
 * - Packed type: (Column[Int], Column[(Int, String)], Option[Double])
 * - Unpacked type: (Int, (Int, String), Option[Double])
 * - Linearized type: (Int, Int, String, Option[Double])
 */
@implicitNotFound(msg = "Don't know how to unpack ${From} to ${To}")
sealed trait =>> [-From, +To] {
  def getTypeMappers(from: From): Vector[TypeMapper[_]]
}

object =>> extends LowPriority_=>> {
  implicit final def unpackJoin[T1 <: AbstractTable[_], T2 <: AbstractTable[_]]: Join[T1, T2] =>> (T1, T2) = new =>> [Join[T1, T2], (T1, T2)] {
    def getTypeMappers(from: Join[T1, T2]) = from.left.getAllColumnTypeMappers
  }

  implicit final def unpackProjection2[T1,T2]: Projection2[T1,T2] =>> (T1,T2) = new =>> [Projection2[T1,T2], (T1,T2)] {
    def getTypeMappers(from: Projection2[T1,T2]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection3[T1,T2,T3]: Projection3[T1,T2,T3] =>> (T1,T2,T3) = new =>> [Projection3[T1,T2,T3], (T1,T2,T3)] {
    def getTypeMappers(from: Projection3[T1,T2,T3]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection4[T1,T2,T3,T4]: Projection4[T1,T2,T3,T4] =>> (T1,T2,T3,T4) = new =>> [Projection4[T1,T2,T3,T4], (T1,T2,T3,T4)] {
    def getTypeMappers(from: Projection4[T1,T2,T3,T4]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection5[T1,T2,T3,T4,T5]: Projection5[T1,T2,T3,T4,T5] =>> (T1,T2,T3,T4,T5) = new =>> [Projection5[T1,T2,T3,T4,T5], (T1,T2,T3,T4,T5)] {
    def getTypeMappers(from: Projection5[T1,T2,T3,T4,T5]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection6[T1,T2,T3,T4,T5,T6]: Projection6[T1,T2,T3,T4,T5,T6] =>> (T1,T2,T3,T4,T5,T6) = new =>> [Projection6[T1,T2,T3,T4,T5,T6], (T1,T2,T3,T4,T5,T6)] {
    def getTypeMappers(from: Projection6[T1,T2,T3,T4,T5,T6]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection7[T1,T2,T3,T4,T5,T6,T7]: Projection7[T1,T2,T3,T4,T5,T6,T7] =>> (T1,T2,T3,T4,T5,T6,T7) = new =>> [Projection7[T1,T2,T3,T4,T5,T6,T7], (T1,T2,T3,T4,T5,T6,T7)] {
    def getTypeMappers(from: Projection7[T1,T2,T3,T4,T5,T6,T7]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection8[T1,T2,T3,T4,T5,T6,T7,T8]: Projection8[T1,T2,T3,T4,T5,T6,T7,T8] =>> (T1,T2,T3,T4,T5,T6,T7,T8) = new =>> [Projection8[T1,T2,T3,T4,T5,T6,T7,T8], (T1,T2,T3,T4,T5,T6,T7,T8)] {
    def getTypeMappers(from: Projection8[T1,T2,T3,T4,T5,T6,T7,T8]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection9[T1,T2,T3,T4,T5,T6,T7,T8,T9]: Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9) = new =>> [Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9], (T1,T2,T3,T4,T5,T6,T7,T8,T9)] {
    def getTypeMappers(from: Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]: Projection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) = new =>> [Projection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] {
    def getTypeMappers(from: Projection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]: Projection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) = new =>> [Projection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] {
    def getTypeMappers(from: Projection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]: Projection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) = new =>> [Projection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] {
    def getTypeMappers(from: Projection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]: Projection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13) = new =>> [Projection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] {
    def getTypeMappers(from: Projection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]: Projection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14) = new =>> [Projection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] {
    def getTypeMappers(from: Projection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]: Projection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15) = new =>> [Projection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] {
    def getTypeMappers(from: Projection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]: Projection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) = new =>> [Projection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] {
    def getTypeMappers(from: Projection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]: Projection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17) = new =>> [Projection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] {
    def getTypeMappers(from: Projection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]: Projection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18) = new =>> [Projection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] {
    def getTypeMappers(from: Projection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]: Projection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19) = new =>> [Projection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] {
    def getTypeMappers(from: Projection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]: Projection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20) = new =>> [Projection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] {
    def getTypeMappers(from: Projection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]: Projection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21) = new =>> [Projection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] {
    def getTypeMappers(from: Projection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]) = from.getAllColumnTypeMappers
  }
  implicit final def unpackProjection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]: Projection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22] =>> (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22) = new =>> [Projection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22], (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] {
    def getTypeMappers(from: Projection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]) = from.getAllColumnTypeMappers
  }

  final type CanUnpack[-T] = T =>> _
  val unpackUnit: Unit =>> Unit = new =>> [Unit, Unit] {
    def getTypeMappers(from: Unit) = Vector[TypeMapper[_]]()
  }

  val unpackNothing: Any =>> Nothing = new =>> [Any, Nothing] {
    def getTypeMappers(from: Any) = throw new SQueryException("Cannot unpack implicitly lifted table")
  }
}

trait LowPriority_=>> {
  implicit final def unpackPrimitive[T](implicit tm: TypeMapper[T]): T =>> T = new =>> [T, T] {
    def getTypeMappers(from: T) = Vector(tm)
  }
  implicit final def unpackColumnBase[T]: ColumnBase[T] =>> T = new =>> [ColumnBase[T], T] {
    def getTypeMappers(from: ColumnBase[T]) = from.getAllColumnTypeMappers
  }

  implicit final def unpackTuple2[T1,T2, U1,U2](implicit u1: T1 =>> U1, u2: T2 =>> U2): (T1,T2) =>> (U1,U2) = new =>> [(T1,T2), (U1,U2)] {
    def getTypeMappers(from: (T1,T2)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2)
  }
  implicit final def unpackTuple3[T1,T2,T3, U1,U2,U3](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3): (T1,T2,T3) =>> (U1,U2,U3) = new =>> [(T1,T2,T3), (U1,U2,U3)] {
    def getTypeMappers(from: (T1,T2,T3)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3)
  }
  implicit final def unpackTuple4[T1,T2,T3,T4, U1,U2,U3,U4](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4): (T1,T2,T3,T4) =>> (U1,U2,U3,U4) = new =>> [(T1,T2,T3,T4), (U1,U2,U3,U4)] {
    def getTypeMappers(from: (T1,T2,T3,T4)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4)
  }
  implicit final def unpackTuple5[T1,T2,T3,T4,T5, U1,U2,U3,U4,U5](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5): (T1,T2,T3,T4,T5) =>> (U1,U2,U3,U4,U5) = new =>> [(T1,T2,T3,T4,T5), (U1,U2,U3,U4,U5)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5)
  }
  implicit final def unpackTuple6[T1,T2,T3,T4,T5,T6, U1,U2,U3,U4,U5,U6](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6): (T1,T2,T3,T4,T5,T6) =>> (U1,U2,U3,U4,U5,U6) = new =>> [(T1,T2,T3,T4,T5,T6), (U1,U2,U3,U4,U5,U6)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6)
  }
  implicit final def unpackTuple7[T1,T2,T3,T4,T5,T6,T7, U1,U2,U3,U4,U5,U6,U7](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7): (T1,T2,T3,T4,T5,T6,T7) =>> (U1,U2,U3,U4,U5,U6,U7) = new =>> [(T1,T2,T3,T4,T5,T6,T7), (U1,U2,U3,U4,U5,U6,U7)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7)
  }
  implicit final def unpackTuple8[T1,T2,T3,T4,T5,T6,T7,T8, U1,U2,U3,U4,U5,U6,U7,U8](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8): (T1,T2,T3,T4,T5,T6,T7,T8) =>> (U1,U2,U3,U4,U5,U6,U7,U8) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8), (U1,U2,U3,U4,U5,U6,U7,U8)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8)
  }
  implicit final def unpackTuple9[T1,T2,T3,T4,T5,T6,T7,T8,T9, U1,U2,U3,U4,U5,U6,U7,U8,U9](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9): (T1,T2,T3,T4,T5,T6,T7,T8,T9) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9), (U1,U2,U3,U4,U5,U6,U7,U8,U9)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9)
  }
  implicit final def unpackTuple10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10)
  }
  implicit final def unpackTuple11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11)
  }
  implicit final def unpackTuple12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12)
  }
  implicit final def unpackTuple13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13)
  }
  implicit final def unpackTuple14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14)
  }
  implicit final def unpackTuple15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15)
  }
  implicit final def unpackTuple16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16)
  }
  implicit final def unpackTuple17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16, u17: T17 =>> U17): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16) ++ u17.getTypeMappers(from._17)
  }
  implicit final def unpackTuple18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16, u17: T17 =>> U17, u18: T18 =>> U18): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16) ++ u17.getTypeMappers(from._17) ++ u18.getTypeMappers(from._18)
  }
  implicit final def unpackTuple19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16, u17: T17 =>> U17, u18: T18 =>> U18, u19: T19 =>> U19): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16) ++ u17.getTypeMappers(from._17) ++ u18.getTypeMappers(from._18) ++ u19.getTypeMappers(from._19)
  }
  implicit final def unpackTuple20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16, u17: T17 =>> U17, u18: T18 =>> U18, u19: T19 =>> U19, u20: T20 =>> U20): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16) ++ u17.getTypeMappers(from._17) ++ u18.getTypeMappers(from._18) ++ u19.getTypeMappers(from._19) ++ u20.getTypeMappers(from._20)
  }
  implicit final def unpackTuple21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16, u17: T17 =>> U17, u18: T18 =>> U18, u19: T19 =>> U19, u20: T20 =>> U20, u21: T21 =>> U21): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16) ++ u17.getTypeMappers(from._17) ++ u18.getTypeMappers(from._18) ++ u19.getTypeMappers(from._19) ++ u20.getTypeMappers(from._20) ++ u21.getTypeMappers(from._21)
  }
  implicit final def unpackTuple22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22, U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21,U22](implicit u1: T1 =>> U1, u2: T2 =>> U2, u3: T3 =>> U3, u4: T4 =>> U4, u5: T5 =>> U5, u6: T6 =>> U6, u7: T7 =>> U7, u8: T8 =>> U8, u9: T9 =>> U9, u10: T10 =>> U10, u11: T11 =>> U11, u12: T12 =>> U12, u13: T13 =>> U13, u14: T14 =>> U14, u15: T15 =>> U15, u16: T16 =>> U16, u17: T17 =>> U17, u18: T18 =>> U18, u19: T19 =>> U19, u20: T20 =>> U20, u21: T21 =>> U21, u22: T22 =>> U22): (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22) =>> (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21,U22) = new =>> [(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22), (U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21,U22)] {
    def getTypeMappers(from: (T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)) =
      u1.getTypeMappers(from._1) ++ u2.getTypeMappers(from._2) ++ u3.getTypeMappers(from._3) ++ u4.getTypeMappers(from._4) ++ u5.getTypeMappers(from._5) ++ u6.getTypeMappers(from._6) ++ u7.getTypeMappers(from._7) ++ u8.getTypeMappers(from._8) ++ u9.getTypeMappers(from._9) ++ u10.getTypeMappers(from._10) ++ u11.getTypeMappers(from._11) ++ u12.getTypeMappers(from._12) ++ u13.getTypeMappers(from._13) ++ u14.getTypeMappers(from._14) ++ u15.getTypeMappers(from._15) ++ u16.getTypeMappers(from._16) ++ u17.getTypeMappers(from._17) ++ u18.getTypeMappers(from._18) ++ u19.getTypeMappers(from._19) ++ u20.getTypeMappers(from._20) ++ u21.getTypeMappers(from._21) ++ u22.getTypeMappers(from._22)
  }
}

case class Unpackable[T, +U](value: T, unpack: T =>> U) {
  def endoMap(f: T => T): Unpackable[T, U] = new Unpackable(f(value), unpack)
  def getTypeMappers = unpack.getTypeMappers(value)
}

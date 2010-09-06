package org.scalaquery.ql

import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.{Node, UnaryNode}

sealed trait Projection[T <: Product] extends ColumnBase[T] with Product {
  type V = T

  def nodeChildren = 0 until productArity map { i => Node(productElement(i)) } toList

  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]) {
    for(i <- 0 until productArity) {
      productElement(i).asInstanceOf[Column[Any]].setParameter(profile, ps, value.map(_.productElement(i)))
    }
  }

  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) {
    for(i <- 0 until productArity) {
      productElement(i).asInstanceOf[Column[Any]].updateResult(profile, rs, value.productElement(i))
    }
  }

  def <>[R](f: (T => R), g: (R => Option[T])): MappedProjection[R,T] = new MappedProjection(this, f, g)

  override def toString = "Projection" + productArity
}

object Projection {
  def unapply[T1,T2](p: Projection2[T1,T2]) = Some(p)
  def unapply[T1,T2,T3](p: Projection3[T1,T2,T3]) = Some(p)
  def unapply[T1,T2,T3,T4](p: Projection4[T1,T2,T3,T4]) = Some(p)
  def unapply[T1,T2,T3,T4,T5](p: Projection5[T1,T2,T3,T4,T5]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6](p: Projection6[T1,T2,T3,T4,T5,T6]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7](p: Projection7[T1,T2,T3,T4,T5,T6,T7]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8](p: Projection8[T1,T2,T3,T4,T5,T6,T7,T8]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9](p: Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](p: Projection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](p: Projection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](p: Projection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](p: Projection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](p: Projection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](p: Projection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](p: Projection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](p: Projection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](p: Projection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](p: Projection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](p: Projection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](p: Projection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]) = Some(p)
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](p: Projection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]) = Some(p)
}

class MappedProjection[T, P <: Product](val child: Projection[P], f: (P => T), g: (T => Option[P])) extends ColumnBase[T] with UnaryNode {
  override def toString = "MappedProjection"
  override def nodeDelegate = if(op eq null) Node(child) else op.nodeDelegate

  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit =
    child.setParameter(profile, ps, value.flatMap(g))

  def getResult(profile: BasicProfile, rs: PositionedResult) = f(child.getResult(profile, rs))
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = child.updateResult(profile, rs, g(value).get)
}

object ~ {
  def unapply[T1,T2](p: Projection2[T1,T2]) =
    Some(p)
  def unapply[T1,T2,T3](p: Projection3[T1,T2,T3]) =
    Some((new Projection2(p._1,p._2), p._3))
  def unapply[T1,T2,T3,T4](p: Projection4[T1,T2,T3,T4]) =
    Some((new Projection3(p._1,p._2,p._3), p._4))
  def unapply[T1,T2,T3,T4,T5](p: Projection5[T1,T2,T3,T4,T5]) =
    Some((new Projection4(p._1,p._2,p._3,p._4), p._5))
  def unapply[T1,T2,T3,T4,T5,T6](p: Projection6[T1,T2,T3,T4,T5,T6]) =
    Some((new Projection5(p._1,p._2,p._3,p._4,p._5), p._6))
  def unapply[T1,T2,T3,T4,T5,T6,T7](p: Projection7[T1,T2,T3,T4,T5,T6,T7]) =
    Some((new Projection6(p._1,p._2,p._3,p._4,p._5,p._6), p._7))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8](p: Projection8[T1,T2,T3,T4,T5,T6,T7,T8]) =
    Some((new Projection7(p._1,p._2,p._3,p._4,p._5,p._6,p._7), p._8))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9](p: Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9]) =
    Some((new Projection8(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8), p._9))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](p: Projection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10]) =
    Some((new Projection9(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9), p._10))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](p: Projection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11]) =
    Some((new Projection10(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10), p._11))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](p: Projection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12]) =
    Some((new Projection11(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11), p._12))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](p: Projection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13]) =
    Some((new Projection12(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12), p._13))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](p: Projection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14]) =
    Some((new Projection13(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13), p._14))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](p: Projection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15]) =
    Some((new Projection14(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14), p._15))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](p: Projection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16]) =
    Some((new Projection15(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15), p._16))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](p: Projection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17]) =
    Some((new Projection16(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16), p._17))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](p: Projection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18]) =
    Some((new Projection17(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17), p._18))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](p: Projection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19]) =
    Some((new Projection18(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18), p._19))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](p: Projection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20]) =
    Some((new Projection19(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18,p._19), p._20))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](p: Projection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21]) =
    Some((new Projection20(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18,p._19,p._20), p._21))
  def unapply[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](p: Projection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22]) =
    Some((new Projection21(p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18,p._19,p._20,p._21), p._22))
}

final class Projection2[T1,T2](
  override val _1: Column[T1],
  override val _2: Column[T2]
)
extends Tuple2(_1,_2) with Projection[(T1,T2)] {
  def ~[U](c: Column[U]) = new Projection3(_1,_2,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection2(
    _1.mapOp(f),
    _2.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2), g)
}

final class Projection3[T1,T2,T3](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3]
)
extends Tuple3(_1,_2,_3) with Projection[(T1,T2,T3)] {
  def ~[U](c: Column[U]) = new Projection4(_1,_2,_3,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection3(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3), g)
}

final class Projection4[T1,T2,T3,T4](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4]
)
extends Tuple4(_1,_2,_3,_4) with Projection[(T1,T2,T3,T4)] {
  def ~[U](c: Column[U]) = new Projection5(_1,_2,_3,_4,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection4(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4), g)
}

final class Projection5[T1,T2,T3,T4,T5](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5]
)
extends Tuple5(_1,_2,_3,_4,_5) with Projection[(T1,T2,T3,T4,T5)] {
  def ~[U](c: Column[U]) = new Projection6(_1,_2,_3,_4,_5,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection5(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5), g)
}

final class Projection6[T1,T2,T3,T4,T5,T6](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6]
)
extends Tuple6(_1,_2,_3,_4,_5,_6) with Projection[(T1,T2,T3,T4,T5,T6)] {
  def ~[U](c: Column[U]) = new Projection7(_1,_2,_3,_4,_5,_6,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection6(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6), g)
}

final class Projection7[T1,T2,T3,T4,T5,T6,T7](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7]
)
extends Tuple7(_1,_2,_3,_4,_5,_6,_7) with Projection[(T1,T2,T3,T4,T5,T6,T7)] {
  def ~[U](c: Column[U]) = new Projection8(_1,_2,_3,_4,_5,_6,_7,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection7(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7), g)
}

final class Projection8[T1,T2,T3,T4,T5,T6,T7,T8](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8]
)
extends Tuple8(_1,_2,_3,_4,_5,_6,_7,_8) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8)] {
  def ~[U](c: Column[U]) = new Projection9(_1,_2,_3,_4,_5,_6,_7,_8,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection8(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8), g)
}

final class Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9]
)
extends Tuple9(_1,_2,_3,_4,_5,_6,_7,_8,_9) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] {
  def ~[U](c: Column[U]) = new Projection10(_1,_2,_3,_4,_5,_6,_7,_8,_9,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection9(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9), g)
}

final class Projection10[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10]
)
extends Tuple10(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10)] {
  def ~[U](c: Column[U]) = new Projection11(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection10(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10), g)
}

final class Projection11[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11]
)
extends Tuple11(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11)] {
  def ~[U](c: Column[U]) = new Projection12(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection11(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11), g)
}

final class Projection12[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12]
)
extends Tuple12(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12)] {
  def ~[U](c: Column[U]) = new Projection13(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection12(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12), g)
}

final class Projection13[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13]
)
extends Tuple13(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13)] {
  def ~[U](c: Column[U]) = new Projection14(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection13(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13), g)
}

final class Projection14[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14]
)
extends Tuple14(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14)] {
  def ~[U](c: Column[U]) = new Projection15(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection14(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14), g)
}

final class Projection15[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15]
)
extends Tuple15(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15)] {
  def ~[U](c: Column[U]) = new Projection16(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection15(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15), g)
}

final class Projection16[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16]
)
extends Tuple16(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16)] {
  def ~[U](c: Column[U]) = new Projection17(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection16(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16), g)
}

final class Projection17[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16],
  override val _17: Column[T17]
)
extends Tuple17(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17)] {
  def ~[U](c: Column[U]) = new Projection18(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs),
    _17.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection17(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f),
    _17.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17), g)
}

final class Projection18[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16],
  override val _17: Column[T17],
  override val _18: Column[T18]
)
extends Tuple18(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18)] {
  def ~[U](c: Column[U]) = new Projection19(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs),
    _17.getResult(profile, rs),
    _18.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection18(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f),
    _17.mapOp(f),
    _18.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18), g)
}

final class Projection19[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16],
  override val _17: Column[T17],
  override val _18: Column[T18],
  override val _19: Column[T19]
)
extends Tuple19(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19)] {
  def ~[U](c: Column[U]) = new Projection20(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs),
    _17.getResult(profile, rs),
    _18.getResult(profile, rs),
    _19.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection19(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f),
    _17.mapOp(f),
    _18.mapOp(f),
    _19.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19), g)
}

final class Projection20[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16],
  override val _17: Column[T17],
  override val _18: Column[T18],
  override val _19: Column[T19],
  override val _20: Column[T20]
)
extends Tuple20(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20)] {
  def ~[U](c: Column[U]) = new Projection21(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs),
    _17.getResult(profile, rs),
    _18.getResult(profile, rs),
    _19.getResult(profile, rs),
    _20.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection20(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f),
    _17.mapOp(f),
    _18.mapOp(f),
    _19.mapOp(f),
    _20.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20), g)
}

final class Projection21[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16],
  override val _17: Column[T17],
  override val _18: Column[T18],
  override val _19: Column[T19],
  override val _20: Column[T20],
  override val _21: Column[T21]
)
extends Tuple21(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21)] {
  def ~[U](c: Column[U]) = new Projection22(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,c)
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs),
    _17.getResult(profile, rs),
    _18.getResult(profile, rs),
    _19.getResult(profile, rs),
    _20.getResult(profile, rs),
    _21.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection21(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f),
    _17.mapOp(f),
    _18.mapOp(f),
    _19.mapOp(f),
    _20.mapOp(f),
    _21.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20,t._21), g)
}

final class Projection22[T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22](
  override val _1: Column[T1],
  override val _2: Column[T2],
  override val _3: Column[T3],
  override val _4: Column[T4],
  override val _5: Column[T5],
  override val _6: Column[T6],
  override val _7: Column[T7],
  override val _8: Column[T8],
  override val _9: Column[T9],
  override val _10: Column[T10],
  override val _11: Column[T11],
  override val _12: Column[T12],
  override val _13: Column[T13],
  override val _14: Column[T14],
  override val _15: Column[T15],
  override val _16: Column[T16],
  override val _17: Column[T17],
  override val _18: Column[T18],
  override val _19: Column[T19],
  override val _20: Column[T20],
  override val _21: Column[T21],
  override val _22: Column[T22]
)
extends Tuple22(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,_17,_18,_19,_20,_21,_22) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22)] {
  
  def getResult(profile: BasicProfile, rs: PositionedResult) = (
    _1.getResult(profile, rs),
    _2.getResult(profile, rs),
    _3.getResult(profile, rs),
    _4.getResult(profile, rs),
    _5.getResult(profile, rs),
    _6.getResult(profile, rs),
    _7.getResult(profile, rs),
    _8.getResult(profile, rs),
    _9.getResult(profile, rs),
    _10.getResult(profile, rs),
    _11.getResult(profile, rs),
    _12.getResult(profile, rs),
    _13.getResult(profile, rs),
    _14.getResult(profile, rs),
    _15.getResult(profile, rs),
    _16.getResult(profile, rs),
    _17.getResult(profile, rs),
    _18.getResult(profile, rs),
    _19.getResult(profile, rs),
    _20.getResult(profile, rs),
    _21.getResult(profile, rs),
    _22.getResult(profile, rs)
  )
  override def mapOp(f: Node => Node): this.type = new Projection22(
    _1.mapOp(f),
    _2.mapOp(f),
    _3.mapOp(f),
    _4.mapOp(f),
    _5.mapOp(f),
    _6.mapOp(f),
    _7.mapOp(f),
    _8.mapOp(f),
    _9.mapOp(f),
    _10.mapOp(f),
    _11.mapOp(f),
    _12.mapOp(f),
    _13.mapOp(f),
    _14.mapOp(f),
    _15.mapOp(f),
    _16.mapOp(f),
    _17.mapOp(f),
    _18.mapOp(f),
    _19.mapOp(f),
    _20.mapOp(f),
    _21.mapOp(f),
    _22.mapOp(f)
  ).asInstanceOf[this.type]
  def <>[R](f: ((T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22) => R), g: (R => Option[V])): MappedProjection[R,V] =
    <>(t => f(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10,t._11,t._12,t._13,t._14,t._15,t._16,t._17,t._18,t._19,t._20,t._21,t._22), g)
}


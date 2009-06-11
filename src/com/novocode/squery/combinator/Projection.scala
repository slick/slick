package com.novocode.squery.combinator

import com.novocode.squery.session.{PositionedResult, PositionedParameters}


sealed trait Projection[T <: Product] extends ConvertibleColumn[T] with Product {
  def nodeChildren = 0 until productArity map { i => Node(productElement(i)) } toList

  def setParameter(ps: PositionedParameters, value: T): Unit = {
    for(i <- 0 until productArity) {
      val v = value.productElement(i)
      productElement(i).asInstanceOf[ConvertibleColumn[Any]].setParameter(ps, v)
    }
  }

  override def toString = "Projection" + productArity
}

final class Projection2[T1,T2](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2])
extends Tuple2(_1,_2) with Projection[(T1,T2)] {
  def ~[U](c: SimpleColumn[U]) = new Projection3(_1,_2,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs))
}

final class Projection3[T1,T2,T3](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3])
extends Tuple3(_1,_2,_3) with Projection[(T1,T2,T3)] {
  def ~[U](c: SimpleColumn[U]) = new Projection4(_1,_2,_3,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs))
}

final class Projection4[T1,T2,T3,T4](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4])
extends Tuple4(_1,_2,_3,_4) with Projection[(T1,T2,T3,T4)] {
  def ~[U](c: SimpleColumn[U]) = new Projection5(_1,_2,_3,_4,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs))
}

final class Projection5[T1,T2,T3,T4,T5](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5])
extends Tuple5(_1,_2,_3,_4,_5) with Projection[(T1,T2,T3,T4,T5)] {
  def ~[U](c: SimpleColumn[U]) = new Projection6(_1,_2,_3,_4,_5,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs))
}

final class Projection6[T1,T2,T3,T4,T5,T6](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6])
extends Tuple6(_1,_2,_3,_4,_5,_6) with Projection[(T1,T2,T3,T4,T5,T6)] {
  def ~[U](c: SimpleColumn[U]) = new Projection7(_1,_2,_3,_4,_5,_6,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs))
}

final class Projection7[T1,T2,T3,T4,T5,T6,T7](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6],
  override val _7: SimpleColumn[T7])
extends Tuple7(_1,_2,_3,_4,_5,_6,_7) with Projection[(T1,T2,T3,T4,T5,T6,T7)] {
  def ~[U](c: SimpleColumn[U]) = new Projection8(_1,_2,_3,_4,_5,_6,_7,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs))
}

final class Projection8[T1,T2,T3,T4,T5,T6,T7,T8](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6],
  override val _7: SimpleColumn[T7],
  override val _8: SimpleColumn[T8])
extends Tuple8(_1,_2,_3,_4,_5,_6,_7,_8) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8)] {
  def ~[U](c: SimpleColumn[U]) = new Projection9(_1,_2,_3,_4,_5,_6,_7,_8,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs),
     _8.getResult(rs))
}

final class Projection9[T1,T2,T3,T4,T5,T6,T7,T8,T9](
  override val _1: SimpleColumn[T1],
  override val _2: SimpleColumn[T2],
  override val _3: SimpleColumn[T3],
  override val _4: SimpleColumn[T4],
  override val _5: SimpleColumn[T5],
  override val _6: SimpleColumn[T6],
  override val _7: SimpleColumn[T7],
  override val _8: SimpleColumn[T8],
  override val _9: SimpleColumn[T9])
extends Tuple9(_1,_2,_3,_4,_5,_6,_7,_8,_9) with Projection[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] {
  //def ~[U](c: SimpleColumn[U]) = new Projection10(_1,_2,_3,_4,_5,_6,_7,_8,_9,c)
  def getResult(rs: PositionedResult) =
    (_1.getResult(rs),
     _2.getResult(rs),
     _3.getResult(rs),
     _4.getResult(rs),
     _5.getResult(rs),
     _6.getResult(rs),
     _7.getResult(rs),
     _8.getResult(rs),
     _9.getResult(rs))
}

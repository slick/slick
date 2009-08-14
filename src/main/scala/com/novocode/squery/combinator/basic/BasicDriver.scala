package com.novocode.squery.combinator.basic

object BasicDriver extends BasicProfile { self =>

  type ImplicitT = BasicImplicitConversions[BasicDriver.type]

  val Implicit = new BasicImplicitConversions[BasicDriver.type] {
    implicit val squeryDriver = self
  }
}

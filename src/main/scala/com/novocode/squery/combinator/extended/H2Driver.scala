package com.novocode.squery.combinator.extended

object H2Driver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[H2Driver.type]

  val Implicit = new ExtendedImplicitConversions[H2Driver.type] {
    implicit val squeryDriver = self
  }
}

package org.scalaquery.ql.basic

object BasicDriver extends BasicProfile { self =>

  type ImplicitT = BasicImplicitConversions[BasicDriver.type]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new BasicImplicitConversions[BasicDriver.type] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}
}

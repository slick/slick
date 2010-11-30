package org.scalaquery.ql.basic

class BasicDriver extends BasicProfile { self =>

  type ImplicitT = BasicImplicitConversions[BasicDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new BasicImplicitConversions[BasicDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}
}

object BasicDriver extends BasicDriver

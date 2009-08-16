package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Query, NamingContext}
import com.novocode.squery.combinator.basic.{BasicQueryBuilder, ConcreteBasicQueryBuilder, BasicTypeMapperDelegates}

object OracleDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[OracleDriver.type]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[OracleDriver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new OracleQueryBuilder(query, nc, None, this)
}

class OracleQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: OracleDriver.type)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  override type Self = OracleQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new OracleQueryBuilder(query, nc, Some(this), profile)

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }
}

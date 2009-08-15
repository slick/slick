package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Query, NamingContext}
import com.novocode.squery.combinator.basic.{BasicQueryBuilder, ConcreteBasicQueryBuilder}

object OracleDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[OracleDriver.type]

  val Implicit = new ExtendedImplicitConversions[OracleDriver.type] {
    implicit val squeryDriver = self
  }

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new OracleQueryBuilder(query, nc, None)
}

class OracleQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder])
extends BasicQueryBuilder(_query, _nc, parent) {

  override type Self = OracleQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new OracleQueryBuilder(query, nc, Some(this))

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }
}

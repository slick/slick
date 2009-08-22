package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Query, NamingContext, Node, SQLBuilder, Operator}
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

  //TODO Support ExtendedOperator.TakeDrop
  /*override protected def buildSelect(value: Node, b: SQLBuilder, rename: Boolean): Unit = value match {
    case Operator.Count(e) =>
      b += "SELECT count(*) from ("; buildSelect(e, b, false); b += ")"
      if(rename) b += " as c1"
    case _ =>
      b += "SELECT "
      expr(value, b, rename)
      fromSlot = b.createSlot
      appendClauses(b)
  }*/

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }
}

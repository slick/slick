package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Query, NamingContext, Node, SQLBuilder, ColumnOps}
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

  import ExtendedQueryOps._

  override type Self = OracleQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new OracleQueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ColumnOps.Concat(l, r) => b += '('; expr(l, b); b += "||"; expr(r, b); b += ')'
    case _ => super.innerExpr(c, b)
  }

  override protected def innerBuildSelect(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(t), None) :: _ =>
        b += "SELECT * FROM (SELECT "
        expr(Node(query.value), b, rename)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") WHERE ROWNUM <= "
        expr(t, b)
      case TakeDrop(to, Some(d)) :: _ =>
        b += "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM ("
        expr(Node(query.value), b, rename)
        b += ",ROWNUM ROWNUM_I"
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") t0) WHERE ROWNUM_O"
        to match {
          case Some(t) =>
            b += " BETWEEN (1+"
            expr(d, b)
            b += ") AND ("
            expr(d, b)
            b += "+"
            expr(t, b)
            b += ")"
          case None =>
            b += ">"
            expr(d, b)
        }
        b += " ORDER BY ROWNUM_I"
      case _ =>
        b += "SELECT "
        expr(Node(query.value), b, rename)
        fromSlot = b.createSlot
        appendClauses(b)
    }
  }

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }
}

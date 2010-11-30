package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._

class OracleDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[OracleDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[OracleDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new OracleQueryBuilder(query, nc, None, this)
}

object OracleDriver extends OracleDriver

class OracleQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: OracleDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = OracleQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new OracleQueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ColumnOps.Concat(l, r) => b += '('; expr(l, b); b += "||"; expr(r, b); b += ')'
    case _ => super.innerExpr(c, b)
  }

  override protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(t), None) :: _ =>
        b += "SELECT * FROM (SELECT "
        expr(Node(query.value), b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") WHERE ROWNUM <= " += t
      case TakeDrop(to, Some(d)) :: _ =>
        b += "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM ("
        expr(Node(query.value), b, rename, true)
        b += ",ROWNUM ROWNUM_I"
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") t0) WHERE ROWNUM_O"
        to match {
          case Some(t) =>
            b += " BETWEEN (1+" += d += ") AND (" += d += "+" += t += ")"
          case None =>
            b += ">" += d
        }
        b += " ORDER BY ROWNUM_I"
      case _ =>
        b += "SELECT "
        expr(Node(query.value), b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
    }
  }

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }
}

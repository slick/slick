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

  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new OracleQueryBuilder(query, nc, None, this)
}

object OracleDriver extends OracleDriver

class OracleQueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: OracleDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = OracleQueryBuilder
  override protected val scalarFrom = Some("DUAL")
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new OracleQueryBuilder(query, nc, Some(this), profile)

  override protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(t), None) :: _ =>
        b += "SELECT * FROM (SELECT "
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") WHERE ROWNUM <= " += t
      case TakeDrop(to, Some(d)) :: _ =>
        b += "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM (SELECT "
        expr(Node(query.reified), b, rename, true)
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
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
    }
  }

  override protected def appendLimitClause(b: SQLBuilder) = ()
}

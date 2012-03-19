package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.ast._

class OracleDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[OracleDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[OracleDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_, _]) = new OracleQueryBuilder(query, this)
}

object OracleDriver extends OracleDriver

class OracleQueryBuilder(query: Query[_, _], profile: OracleDriver) extends BasicQueryBuilder(query, profile) {

  import ExtendedQueryOps._

  override protected val scalarFrom = Some("DUAL")
  override protected val concatOperator = Some("||")

  override protected def innerBuildSelectNoRewrite(rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(t), None) :: _ =>
        b += "SELECT * FROM (SELECT "
        expr(query.reified)
        //TODO fromSlot = b.createSlot
        appendClauses()
        b += ") WHERE ROWNUM <= " += t
      case TakeDrop(to, Some(d)) :: _ =>
        b += "SELECT * FROM (SELECT t0.*, ROWNUM ROWNUM_O FROM (SELECT "
        expr(Node(query.reified))
        b += ",ROWNUM ROWNUM_I"
        //TODO fromSlot = b.createSlot
        appendClauses()
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
        expr(query.reified)
        //TODO fromSlot = b.createSlot
        appendClauses()
    }
  }

  override protected def appendTakeDropClause(take: Option[Int], drop: Option[Int]) = ()
}

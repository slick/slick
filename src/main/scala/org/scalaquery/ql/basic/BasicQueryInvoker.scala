package org.scalaquery.ql.basic

import java.sql.PreparedStatement
import org.scalaquery.{UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker}
import org.scalaquery.ql.Query
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.util.ValueLinearizer

class BasicQueryInvoker[Q, R](q: Query[Q, R], profile: BasicProfile)
  extends MutatingStatementInvoker[Unit, R] with UnitInvokerMixin[R] with MutatingUnitInvoker[R] {

  override protected val delegate = this

  protected lazy val sres = profile.buildSelectStatement(q)

  def selectStatement = getStatement

  protected def getStatement = sres.sql

  protected def setParam(param: Unit, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), null)

  protected def extractValue(rs: PositionedResult): R = sres.linearizer.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = sres.linearizer.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)

  def invoker: this.type = this
}

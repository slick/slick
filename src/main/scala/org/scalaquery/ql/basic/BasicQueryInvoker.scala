package org.scalaquery.ql.basic

import java.sql.PreparedStatement
import org.scalaquery.{UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker}
import org.scalaquery.ql.{Query, ColumnBase}
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.util.{ValueLinearizer, NamingContext}

class BasicQueryInvoker[Q, R](q: Query[Q, R], profile: BasicProfile)
  extends MutatingStatementInvoker[Unit, R] with UnitInvokerMixin[R] with MutatingUnitInvoker[R] {

  override protected val delegate = this

  protected lazy val (built, lin) = profile.buildSelectStatement(q, NamingContext())

  def selectStatement = getStatement

  protected def getStatement = built.sql

  protected def setParam(param: Unit, st: PreparedStatement): Unit = built.setter(new PositionedParameters(st), null)

  protected def extractValue(rs: PositionedResult): R = lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)

  def invoker: this.type = this
}

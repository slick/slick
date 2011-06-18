package org.scalaquery.ql.basic

import java.sql.PreparedStatement
import org.scalaquery.{SQueryException, MutatingStatementInvoker}
import org.scalaquery.ql.{Query, ColumnBase}
import org.scalaquery.session.{PositionedParameters, PositionedResult}
import org.scalaquery.util.{ValueLinearizer, NamingContext}

class BasicQueryTemplate[P, R](query: Query[_, R], profile: BasicProfile) extends MutatingStatementInvoker[P, R] {

  protected lazy val (built, lin) = profile.buildSelectStatement(query, NamingContext())

  def selectStatement = getStatement

  protected def getStatement = built.sql

  protected def setParam(param: P, st: PreparedStatement): Unit = built.setter(new PositionedParameters(st), param)

  protected def extractValue(rs: PositionedResult): R = lin.asInstanceOf[ValueLinearizer[R]].getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = lin.asInstanceOf[ValueLinearizer[R]].updateResult(profile, rs, value)
}

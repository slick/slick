package org.scalaquery.simple

import java.sql.PreparedStatement
import java.sql.{Date, Time, Timestamp}
import org.scalaquery.{StatementInvoker, UnitInvokerMixin, SQueryException}
import org.scalaquery.session.{PositionedParameters, PositionedResult}

/**
 * Invoker for static queries, i.e. queries with a fixed query string.
 * They may still contain bind variables to be supplied at the call site.
 * The companion object contains utility methods for building static queries.
 */
class StaticQuery[-P,+R](query: String, rconv: GetResult[R], pconv: SetParameter[P])
extends StatementInvoker[P,R] {
  protected def getStatement = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, new PositionedParameters(st))
  protected def extractValue(rs: PositionedResult): R = rconv(rs)
}

object StaticQuery {

  def query[P,R](query: String)(implicit rconv: GetResult[R], pconv: SetParameter[P]) =
    new StaticQuery[P, R](query, rconv, pconv)

  def queryNA[R](query: String)(implicit conv: GetResult[R]) =
    new StaticQuery[Unit, R](query, conv, SetParameter.SetUnit) with UnitInvokerMixin[R]

  def update[P](query: String)(implicit pconv: SetParameter[P]) =
    new StaticQuery[P, Int](query, GetResult.GetUpdateValue, pconv)

  def updateNA(query: String) =
    new StaticQuery[Unit, Int](query, GetResult.GetUpdateValue, SetParameter.SetUnit) with UnitInvokerMixin[Int]
}

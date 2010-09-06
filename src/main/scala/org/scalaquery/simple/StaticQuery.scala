package org.scalaquery.simple

import java.sql.PreparedStatement
import org.scalaquery.{StatementInvoker, UnitInvokerMixin, SQueryException}
import org.scalaquery.session.PositionedResult

/**
 * Invoker for static queries, i.e. queries with a fixed query string.
 * They may still contain bind variables to be supplied at the call site.
 * The companion object contains utility methods for building static queries.
 */
class StaticQuery[-P,+R](query: String, rconv: PositionedResult => R, pconv: (P, PreparedStatement) => Unit)
extends StatementInvoker[P,R] {
  protected def getStatement = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, st)
  protected def extractValue(rs: PositionedResult): R = rconv(rs)
}

object StaticQuery {

  def query[P,R](query: String)(implicit rconv: PositionedResult => R, pconv: (P, PreparedStatement) => Unit) =
    new StaticQuery[P, R](query, rconv, pconv)

  def queryNA[R](query: String)(implicit conv: PositionedResult => R) =
    new StaticQuery[Unit, R](query, conv, Implicit.prepareFromUnit) with UnitInvokerMixin[R]

  def update[P](query: String)(implicit pconv: (P, PreparedStatement) => Unit) =
    new StaticQuery[P, Int](query, updateValueExtractor, pconv)

  def updateNA(query: String) =
    new StaticQuery[Unit, Int](query, updateValueExtractor, Implicit.prepareFromUnit) with UnitInvokerMixin[Int]

  private[this] val updateValueExtractor = { pr: PositionedResult =>
    throw new SQueryException("Update statements should not return a ResultSet")
  }
}

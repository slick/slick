package com.novocode.squery.simple

import java.sql.PreparedStatement
import com.novocode.squery.session.PositionedResult

/**
 * Base class for static queries, i.e. queries with a fixed query string.
 * They may still contain bind variables to be supplied at the call site.
 * The companion object contains utility methods for building static queries.
 */
abstract class StaticQueryBase[-P,+R](query: String, pconv: (P, PreparedStatement) => Unit) extends StatementInvoker[P,R] {
  protected def getStatement = query
  protected def setParam(param: P, st: PreparedStatement) = pconv(param, st)
}

object StaticQueryBase {

  def query[P,R](query: String)(implicit rconv: PositionedResult => R, pconv: (P, PreparedStatement) => Unit) =
    new StaticQuery[P,R](query, rconv, pconv)

  def queryNA[R](query: String)(implicit conv: PositionedResult => R) =
    new StaticQuery[Unit,R](query, conv, Implicit.prepareFromUnit) with NoArgsInvoker[R]

  def update[P](query: String)(implicit pconv: (P, PreparedStatement) => Unit) =
    new StaticUpdate[P](query, pconv)

  def updateNA(query: String) =
    new StaticUpdate[Unit](query, Implicit.prepareFromUnit) with NoArgsInvoker[Int]
}

class StaticQuery[-P,+R](query: String, rconv: PositionedResult => R, pconv: (P, PreparedStatement) => Unit)
extends StaticQueryBase[P,R](query, pconv) {
  protected def extractValue(rs: PositionedResult): R = rconv(rs)
}

class StaticUpdate[-P](query: String, pconv: (P, PreparedStatement) => Unit)
extends StaticQueryBase[P,Int](query, pconv) {
  protected def extractValue(rs: PositionedResult): Int =
    throw new SQueryException("StaticUpdate.extractValue called; Non-query statements should not return a ResultSet")
}

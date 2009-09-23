package com.novocode.squery.combinator.basic

import java.sql.PreparedStatement
import com.novocode.squery.{SQueryException, MutatingStatementInvoker}
import com.novocode.squery.combinator.{Query, ColumnBase, NamingContext}
import com.novocode.squery.session.{PositionedParameters, PositionedResult}

class BasicQueryTemplate[P, R](query: Query[ColumnBase[R]], profile: BasicProfile) extends MutatingStatementInvoker[P, R] {

  protected lazy val built = profile.buildSelectStatement(query, NamingContext())

  def selectStatement = getStatement

  protected def getStatement = built.sql

  protected def setParam(param: P, st: PreparedStatement): Unit = built.setter(new PositionedParameters(st), param)

  protected def extractValue(rs: PositionedResult): R = query.value.getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = query.value.updateResult(profile, rs, value)
}

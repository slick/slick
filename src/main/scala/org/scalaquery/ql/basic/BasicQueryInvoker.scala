package org.scalaquery.ql.basic

import java.sql.PreparedStatement
import org.scalaquery.StatementInvoker
import org.scalaquery.{Invoker, MappedInvoker, UnitInvokerMixin, MutatingStatementInvoker, DelegatingMutatingUnitInvoker}
import org.scalaquery.ql.{Query, ColumnBase}
import org.scalaquery.session.{Session, PositionedParameters, PositionedResult, ReadAheadIterator}
import org.scalaquery.util.{CloseableIterator, NamingContext}

class BasicQueryInvoker[R](q: Query[ColumnBase[R]], profile: BasicProfile)
  extends MutatingStatementInvoker[Unit, R] with UnitInvokerMixin[R] with DelegatingMutatingUnitInvoker[Unit, R] {

  override protected val delegate = this

  protected lazy val built = profile.buildSelectStatement(q, NamingContext())

  def selectStatement = getStatement

  protected def getStatement = built.sql

  protected def setParam(param: Unit, st: PreparedStatement): Unit = built.setter(new PositionedParameters(st), null)

  protected def extractValue(rs: PositionedResult): R = q.value.getResult(profile, rs)

  protected def updateRowValues(rs: PositionedResult, value: R) = q.value.updateResult(profile, rs, value)
}

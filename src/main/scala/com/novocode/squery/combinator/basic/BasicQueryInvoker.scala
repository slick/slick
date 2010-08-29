package com.novocode.squery.combinator.basic

import java.sql.PreparedStatement
import com.novocode.squery.StatementInvoker
import com.novocode.squery.{Invoker, MappedInvoker, UnitInvokerMixin, MutatingStatementInvoker, DelegatingMutatingUnitInvoker}
import com.novocode.squery.combinator.{Query, ColumnBase}
import com.novocode.squery.session.{Session, PositionedParameters, PositionedResult, ReadAheadIterator, CloseableIterator}
import com.novocode.squery.util.NamingContext

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

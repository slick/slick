package com.novocode.squery.combinator

import java.sql.PreparedStatement
import com.novocode.squery.{Invoker, MappedInvoker, UnitInvokerMixin}
import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{Session, PositionedParameters, PositionedResult, ReadAheadIterator, CloseableIterator}

class StatementCombinatorQueryInvoker[+R](q: Query[ColumnBase[R]])
  extends StatementInvoker[Unit, R] with UnitInvokerMixin[R] {

  private lazy val built = QueryBuilder.buildSelect(q, NamingContext())

  def selectStatement = getStatement

  protected def getStatement = built.sql

  protected def setParam(param: Unit, st: PreparedStatement): Unit = built.setter(new PositionedParameters(st), null)

  protected def extractValue(rs: PositionedResult): R = q.value.getResult(rs)
}

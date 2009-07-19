package com.novocode.squery.combinator

import java.sql.PreparedStatement
import com.novocode.squery.{Invoker, MappedInvoker, NoArgsInvoker}
import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{Session, PositionedParameters, PositionedResult, ReadAheadIterator, CloseableIterator}

trait CombinatorQueryInvoker[+R] extends NoArgsInvoker[R] {
  def mapResult[U](f: (R => U)): CombinatorQueryInvoker[U] =
    new MappedInvoker(this, f) with CombinatorQueryInvoker[U]
}

class StatementCombinatorQueryInvoker[+R](q: Query[ColumnBase[R]])
  extends StatementInvoker[Unit, R] with CombinatorQueryInvoker[R] {

  private lazy val built = QueryBuilder.buildSelect(q, NamingContext())

  def selectStatement = getStatement

  protected def getStatement = built._1

  protected def setParam(param: Unit, st: PreparedStatement): Unit = built._2(new PositionedParameters(st))

  protected def extractValue(rs: PositionedResult): R = q.value.getResult(rs)
}

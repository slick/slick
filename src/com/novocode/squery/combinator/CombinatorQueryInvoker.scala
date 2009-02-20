package com.novocode.squery.combinator

import java.sql.PreparedStatement
import com.novocode.squery.{Invoker, MappedInvoker, NoArgsInvoker}
import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{Session, PositionedResult, ReadAheadIterator, CloseableIterator}

trait CombinatorQueryInvoker[+R] extends NoArgsInvoker[R] {
  def mapResult[U](f: (R => U)): CombinatorQueryInvoker[U] =
    new MappedInvoker(this, f) with CombinatorQueryInvoker[U]
}

class StatementCombinatorQueryInvoker[+R](q: Query[ConvertibleColumn[R]])
  extends StatementInvoker[Unit, R] with CombinatorQueryInvoker[R] {

  lazy val selectStatement = new QueryBuilder(q).buildSelect

  protected def getStatement = selectStatement

  protected def setParam(param: Unit, st: PreparedStatement) = ()

  protected def extractValue(rs: PositionedResult): R = q.value.getResult(rs)
}

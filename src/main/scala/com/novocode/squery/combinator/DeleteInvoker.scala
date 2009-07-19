package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{PositionedParameters, Session, CloseableIterator, ReadAheadIterator}

class DeleteInvoker[T] (query: Query[Table[T]]) {

  private lazy val built = QueryBuilder.buildDelete(query, NamingContext())

  def deleteStatement = built._1

  def delete(implicit session: Session): Int = {
    val st = session.allocPS(deleteStatement)
    try {
      built._2(new PositionedParameters(st))
      st.executeUpdate
    } finally session.freePS(deleteStatement, st)
  }
}

package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator}

class DeleteInvoker[T] (query: Query[Table[T]]) {

  lazy val deleteStatement = new QueryBuilder(query).buildDelete

  def delete(implicit session: Session): Int = {
    val st = session.allocPS(deleteStatement)
    try { st.executeUpdate } finally session.freePS(deleteStatement, st)
  }
}

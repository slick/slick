package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.QueryBuilder
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class CombinatorUpdateInvoker[T] (query: Query[Projection[T]]) {

  lazy val updateStatement = QueryBuilder.buildUpdate(query, NamingContext())

  def update(value: T)(implicit session: Session): Int = session.withPS(updateStatement) { st =>
    st.clearParameters
    query.value.setParameter(new PositionedParameters(st), Some(value))
    st.executeUpdate
  }
}

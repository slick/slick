package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.Query
import com.novocode.squery.session.{PositionedParameters, Session, CloseableIterator, ReadAheadIterator}
import com.novocode.squery.util.NamingContext

class BasicDeleteInvoker[T] (query: Query[AbstractBasicTable[T]], profile: BasicProfile) {

  protected lazy val built = profile.buildDeleteStatement(query, NamingContext())

  def deleteStatement = built.sql

  def delete(implicit session: Session): Int = session.withPreparedStatement(deleteStatement) { st =>
    built.setter(new PositionedParameters(st), null)
    st.executeUpdate
  }
}

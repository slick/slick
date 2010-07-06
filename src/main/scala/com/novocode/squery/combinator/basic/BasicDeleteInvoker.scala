package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{Query, NamingContext}
import com.novocode.squery.session.{PositionedParameters, Session, CloseableIterator, ReadAheadIterator}

class BasicDeleteInvoker[T] (query: Query[BasicTable[T]], profile: BasicProfile) {

  protected lazy val built = profile.buildDeleteStatement(query, NamingContext())

  def deleteStatement = built.sql

  def delete(implicit session: Session): Int = session.withPreparedStatement(deleteStatement) { st =>
    built.setter(new PositionedParameters(st), null)
    st.executeUpdate
  }
}

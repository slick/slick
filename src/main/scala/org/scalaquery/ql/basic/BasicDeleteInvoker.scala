package org.scalaquery.ql.basic

import org.scalaquery.ql.Query
import org.scalaquery.session.{PositionedParameters, Session, CloseableIterator, ReadAheadIterator}
import org.scalaquery.util.NamingContext

class BasicDeleteInvoker[T] (query: Query[AbstractBasicTable[T]], profile: BasicProfile) {

  protected lazy val built = profile.buildDeleteStatement(query, NamingContext())

  def deleteStatement = built.sql

  def delete(implicit session: Session): Int = session.withPreparedStatement(deleteStatement) { st =>
    built.setter(new PositionedParameters(st), null)
    st.executeUpdate
  }
}

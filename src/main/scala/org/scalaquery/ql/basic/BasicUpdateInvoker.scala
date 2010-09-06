package org.scalaquery.ql.basic

import org.scalaquery.ql.{Query, Projection, ColumnBase}
import org.scalaquery.session.{Session, ReadAheadIterator, PositionedParameters}
import org.scalaquery.util.{CloseableIterator, NamingContext}

class BasicUpdateInvoker[T] (query: Query[ColumnBase[T]], profile: BasicProfile) {

  protected lazy val built = profile.buildUpdateStatement(query, NamingContext())

  def updateStatement = getStatement

  protected def getStatement = built.sql

  def update(value: T)(implicit session: Session): Int = session.withPreparedStatement(updateStatement) { st =>
    st.clearParameters
    built.setter(new PositionedParameters(st), value)
    st.executeUpdate
  }
}

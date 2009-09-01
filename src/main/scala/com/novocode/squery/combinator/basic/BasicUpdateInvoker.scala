package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{Query, Projection, NamingContext, ColumnBase}
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

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

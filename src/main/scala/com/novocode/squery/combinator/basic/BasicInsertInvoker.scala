package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{ColumnBase, Query}
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class BasicInsertInvoker[T] (column: ColumnBase[T], profile: BasicProfile) {

  lazy val insertStatement = profile.buildInsertStatement(column)
  def insertStatementFor(query: Query[ColumnBase[T]]): String = profile.buildInsertStatement(column, query).sql
  def insertStatementFor(c: ColumnBase[T]): String = insertStatementFor(Query(c))

  def insert(value: T)(implicit session: Session): Int = session.withPreparedStatement(insertStatement) { st =>
    st.clearParameters
    column.setParameter(profile, new PositionedParameters(st), Some(value))
    st.executeUpdate
  }

  def insertAll(values: T*)(implicit session: Session): Int = (0 /: values) { _ + insert(_) }

  def insert(query: Query[ColumnBase[T]])(implicit session: Session): Int = {
    val sbr = profile.buildInsertStatement(column, query)
    session.withPreparedStatement(insertStatementFor(query)) { st =>
      st.clearParameters
      sbr.setter(new PositionedParameters(st), null)
      st.executeUpdate
    }
  }

  def insert(c: ColumnBase[T])(implicit session: Session): Int = insert(Query(c))(session)
}

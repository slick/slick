package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.ColumnBase
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class BasicInsertInvoker[T] (column: ColumnBase[T], profile: BasicProfile) {

  lazy val insertStatement = profile.buildInsertStatement(column)

  def insert(value: T)(implicit session: Session): Int = session.withPS(insertStatement) { st =>
    st.clearParameters
    column.setParameter(new PositionedParameters(st), Some(value))
    st.executeUpdate
  }

  def insertAll(values: T*)(implicit session: Session): Int = (0 /: values) { _ + insert(_) }
}

package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.InsertUpdateBuilder
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class CombinatorInsertUpdateInvoker[T] (column: ConvertibleColumn[T]) {

  lazy val insertStatement = new InsertUpdateBuilder(column).buildInsert

  def insert(value: T)(implicit session: Session): Int = {
    val st = session.allocPS(insertStatement)
    st.clearParameters
    column.setParameter(new PositionedParameters(st), value)
    try { st.executeUpdate } finally session.freePS(insertStatement, st)
  }

  def insertAll(values: T*)(implicit session: Session): Int = (0 /: values) { _ + insert(_) }
}

package com.novocode.squery.combinator

import com.novocode.squery.combinator.sql.InsertBuilder
import com.novocode.squery.session.{Session, CloseableIterator, ReadAheadIterator, PositionedParameters}

class CombinatorInsertInvoker[T] (column: ConvertibleColumn[T]) {

  lazy val insertStatement = new InsertBuilder(column).buildInsert

  def insert(value: T)(implicit session: Session): Int = {
    val st = session.allocPS(insertStatement)
    try {
      st.clearParameters
      column.setParameter(new PositionedParameters(st), Some(value))
      st.executeUpdate
    } finally session.freePS(insertStatement, st)
  }

  def insertAll(values: T*)(implicit session: Session): Int = (0 /: values) { _ + insert(_) }
}

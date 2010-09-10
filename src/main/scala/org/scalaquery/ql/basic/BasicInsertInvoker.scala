package org.scalaquery.ql.basic

import java.sql.Statement
import org.scalaquery.SQueryException
import org.scalaquery.ql.{ColumnBase, Query}
import org.scalaquery.session.{Session, ReadAheadIterator, PositionedParameters}
import org.scalaquery.util.CloseableIterator

class BasicInsertInvoker[T] (column: ColumnBase[T], profile: BasicProfile) {

  lazy val insertStatement = profile.buildInsertStatement(column)
  def insertStatementFor(query: Query[ColumnBase[T]]): String = profile.buildInsertStatement(column, query).sql
  def insertStatementFor(c: ColumnBase[T]): String = insertStatementFor(Query(c))

  def useBatchUpdates(implicit session: Session) = session.capabilities.supportsBatchUpdates

  /**
   * Insert a single row.
   */
  def insert(value: T)(implicit session: Session): Int = session.withPreparedStatement(insertStatement) { st =>
    st.clearParameters()
    column.setParameter(profile, new PositionedParameters(st), Some(value))
    st.executeUpdate()
  }

  /**
   * Insert multiple rows. Uses JDBC's batch update feature if supported by
   * the JDBC driver. Returns Some(rowsAffected), or None if the database
   * returned no row count for some part of the batch. If any part of the
   * batch fails, an exception thrown.
   */
  def insertAll(values: T*)(implicit session: Session): Option[Int] = {
    if(!useBatchUpdates || (values.isInstanceOf[IndexedSeq[_]] && values.length < 2))
      Some( (0 /: values) { _ + insert(_) } )
    else session.withTransaction {
      session.withPreparedStatement(insertStatement) { st =>
        st.clearParameters()
        for(value <- values) {
          column.setParameter(profile, new PositionedParameters(st), Some(value))
          st.addBatch()
        }
        var unknown = false
        var count = 0
        for((res, idx) <- st.executeBatch().zipWithIndex) res match {
          case Statement.SUCCESS_NO_INFO => unknown = true
          case Statement.EXECUTE_FAILED =>
            throw new SQueryException("Failed to insert row #" + (idx+1))
          case i => count += i
        }
        if(unknown) None else Some(count)
      }
    }
  }

  def insert(query: Query[ColumnBase[T]])(implicit session: Session): Int = {
    val sbr = profile.buildInsertStatement(column, query)
    session.withPreparedStatement(insertStatementFor(query)) { st =>
      st.clearParameters()
      sbr.setter(new PositionedParameters(st), null)
      st.executeUpdate()
    }
  }

  def insert(c: ColumnBase[T])(implicit session: Session): Int = insert(Query(c))(session)
}

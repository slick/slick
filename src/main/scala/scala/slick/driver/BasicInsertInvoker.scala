package scala.slick.driver

import java.sql.Statement
import scala.slick.SLICKException
import scala.slick.ql.{Query, Shape, ShapedValue}
import scala.slick.session.{Session, PositionedParameters}
import scala.slick.util.RecordLinearizer

class BasicInsertInvoker[T, U] (unpackable: ShapedValue[T, U], profile: BasicProfile) {

  lazy val insertStatement = profile.buildInsertStatement(unpackable.value)
  def insertStatementFor[TT](query: Query[TT, U]): String = profile.buildInsertStatement(unpackable.value, query).sql
  def insertStatementFor[TT](c: TT)(implicit shape: Shape[TT, U, _]): String = insertStatementFor(Query(c)(shape))

  def useBatchUpdates(implicit session: Session) = session.capabilities.supportsBatchUpdates

  /**
   * Insert a single row.
   */
  def insert(value: U)(implicit session: Session): Int = session.withPreparedStatement(insertStatement) { st =>
    st.clearParameters()
    unpackable.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[U]].setParameter(profile, new PositionedParameters(st), Some(value))
    st.executeUpdate()
  }

  def insertExpr[TT](c: TT)(implicit shape: Shape[TT, U, _], session: Session): Int =
    insert(Query(c)(shape))(session)

  /**
   * Insert multiple rows. Uses JDBC's batch update feature if supported by
   * the JDBC driver. Returns Some(rowsAffected), or None if the database
   * returned no row count for some part of the batch. If any part of the
   * batch fails, an exception thrown.
   */
  def insertAll(values: U*)(implicit session: Session): Option[Int] = {
    if(!useBatchUpdates || (values.isInstanceOf[IndexedSeq[_]] && values.length < 2))
      Some( (0 /: values) { _ + insert(_) } )
    else session.withTransaction {
      session.withPreparedStatement(insertStatement) { st =>
        st.clearParameters()
        for(value <- values) {
          unpackable.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[U]].setParameter(profile, new PositionedParameters(st), Some(value))
          st.addBatch()
        }
        var unknown = false
        var count = 0
        for((res, idx) <- st.executeBatch().zipWithIndex) res match {
          case Statement.SUCCESS_NO_INFO => unknown = true
          case Statement.EXECUTE_FAILED =>
            throw new SLICKException("Failed to insert row #" + (idx+1))
          case i => count += i
        }
        if(unknown) None else Some(count)
      }
    }
  }

  def insert[TT](query: Query[TT, U])(implicit session: Session): Int = {
    val sbr = profile.buildInsertStatement(unpackable.value, query)
    session.withPreparedStatement(insertStatementFor(query)) { st =>
      st.clearParameters()
      sbr.setter(new PositionedParameters(st), null)
      st.executeUpdate()
    }
  }

  def insertInvoker: this.type = this
}

package scala.slick.driver

import java.sql.{Statement, PreparedStatement}
import scala.slick.SlickException
import scala.slick.lifted.{Query, Shape, ShapedValue}
import scala.slick.session.{Session, PositionedParameters, PositionedResult}
import scala.slick.util.RecordLinearizer
import scala.slick.jdbc.{UnitInvokerMixin, MutatingStatementInvoker, MutatingUnitInvoker}

trait BasicInvokerComponent { driver: BasicDriver =>

  /** Invoker for executing queries. */
  class QueryInvoker[QQ, R](q: Query[QQ, _ <: R])
    extends MutatingStatementInvoker[Unit, R] with UnitInvokerMixin[R] with MutatingUnitInvoker[R] {
    override protected val delegate = this
    protected lazy val sres = buildSelectStatement(q)
    def selectStatement = getStatement
    protected def getStatement = sres.sql
    protected def setParam(param: Unit, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), null)
    protected def extractValue(rs: PositionedResult): R = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].getResult(driver, rs)
    protected def updateRowValues(rs: PositionedResult, value: R) = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[R]].updateResult(driver, rs, value)
    def invoker: this.type = this
  }

  /** Pseudo-invoker for runing DELETE calls. */
  class DeleteInvoker(query: Query[_, _]) {
    protected lazy val built = buildDeleteStatement(query)

    def deleteStatement = built.sql

    def delete(implicit session: Session): Int = session.withPreparedStatement(deleteStatement) { st =>
      built.setter(new PositionedParameters(st), null)
      st.executeUpdate
    }

    def deleteInvoker: this.type = this
  }

  /** Pseudo-invoker for runing INSERT calls. */
  class InsertInvoker[U](unpackable: ShapedValue[_, U]) {
    lazy val insertStatement = buildInsertStatement(unpackable.value)
    def insertStatementFor[TT](query: Query[TT, U]): String = buildInsertStatement(unpackable.value, query).sql
    def insertStatementFor[TT](c: TT)(implicit shape: Shape[TT, U, _]): String = insertStatementFor(Query(c)(shape))

    def useBatchUpdates(implicit session: Session) = session.capabilities.supportsBatchUpdates

    /**
     * Insert a single row.
     */
    def insert(value: U)(implicit session: Session): Int = session.withPreparedStatement(insertStatement) { st =>
      st.clearParameters()
      unpackable.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[U]].setParameter(driver, new PositionedParameters(st), Some(value))
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
            unpackable.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[U]].setParameter(driver, new PositionedParameters(st), Some(value))
            st.addBatch()
          }
          var unknown = false
          var count = 0
          for((res, idx) <- st.executeBatch().zipWithIndex) res match {
            case Statement.SUCCESS_NO_INFO => unknown = true
            case Statement.EXECUTE_FAILED =>
              throw new SlickException("Failed to insert row #" + (idx+1))
            case i => count += i
          }
          if(unknown) None else Some(count)
        }
      }
    }

    def insert[TT](query: Query[TT, U])(implicit session: Session): Int = {
      val sbr = buildInsertStatement(unpackable.value, query)
      session.withPreparedStatement(insertStatementFor(query)) { st =>
        st.clearParameters()
        sbr.setter(new PositionedParameters(st), null)
        st.executeUpdate()
      }
    }

    def insertInvoker: this.type = this
  }

  /** Pseudo-invoker for runing UPDATE calls. */
  class UpdateInvoker[T] (query: Query[_, T]) {
    protected lazy val built = buildUpdateStatement(query)

    def updateStatement = getStatement

    protected def getStatement = built.sql

    def update(value: T)(implicit session: Session): Int = session.withPreparedStatement(updateStatement) { st =>
      st.clearParameters
      val pp = new PositionedParameters(st)
      built.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[T]].setParameter(driver, pp, Some(value))
      built.setter(pp, null)
      st.executeUpdate
    }

    def updateInvoker: this.type = this
  }
}

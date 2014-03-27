package scala.slick.jdbc

import java.sql.ResultSet

/** Represents a result set holdability mode .*/
sealed abstract class ResultSetHoldability(val intValue: Int) { self =>
  /** Run a block of code on top of a JDBC session with this concurrency mode */
  def apply[T](base: JdbcBackend#Session)(f: JdbcBackend#Session => T): T = f(base.forParameters(rsHoldability = self))

  /** Run a block of code on top of the dynamic, thread-local JDBC session with this holdability mode */
  def apply[T](f: => T)(implicit base: JdbcBackend#Session): T = apply(base)(_.asDynamicSession(f))

  /** Return this `ResultSetHoldability`, unless it is `Auto` in which case
    * the specified holdability mode is returned instead. */
  def withDefault(r: ResultSetHoldability) = this
}

object ResultSetHoldability {
  /** The current holdability mode of the JDBC driver */
  case object Auto extends ResultSetHoldability(0) {
    override def withDefault(r: ResultSetHoldability) = r
  }

  /** The default holdability mode of the JDBC driver */
  case object Default extends ResultSetHoldability(0)

  /** The holdability mode which indicates that result sets remain open when the
    * current transaction is committed. */
  case object HoldCursorsOverCommit extends ResultSetHoldability(ResultSet.HOLD_CURSORS_OVER_COMMIT)

  /** The holdability mode which indicates that result sets are closed when the
    * current transaction is committed. */
  case object CloseCursorsAtCommit extends ResultSetHoldability(ResultSet.CLOSE_CURSORS_AT_COMMIT)
}

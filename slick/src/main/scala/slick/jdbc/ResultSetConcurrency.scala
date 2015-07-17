package slick.jdbc

import java.sql.ResultSet

/** Represents a result set concurrency mode. */
sealed abstract class ResultSetConcurrency(val intValue: Int) { self =>
  /** Return this `ResultSetConcurrency`, unless it is `Auto` in which case
    * the specified concurrency mode is returned instead. */
  def withDefault(r: ResultSetConcurrency) = this
}

object ResultSetConcurrency {
  /** The current concurrency mode of the JDBC driver */
  case object Auto extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY) {
    override def withDefault(r: ResultSetConcurrency) = r
  }

  /** The concurrency mode which indicates that the result set may <em>not</em> be updated. */
  case object ReadOnly extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY)

  /** The concurrency mode which indicates that the result set may be updated. */
  case object Updatable extends ResultSetConcurrency(ResultSet.CONCUR_UPDATABLE)
}
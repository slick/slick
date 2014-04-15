package scala.slick.jdbc

import java.sql.ResultSet

/** Represents a result set concurrency mode. */
sealed abstract class ResultSetConcurrency(val intValue: Int) { self =>
  /** Run a block of code on top of a JDBC session with this concurrency mode */
  def apply[T](base: JdbcBackend#Session)(f: JdbcBackend#Session => T): T = f(base.forParameters(rsConcurrency = self))

  /** Run a block of code on top of the dynamic, thread-local JDBC session with this concurrency mode */
  def apply[T](f: => T)(implicit base: JdbcBackend#Session): T = apply(base)(_.asDynamicSession(f))

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
package slick.jdbc

import java.sql.Connection
import slick.SlickException
import slick.util.??

/** A JDBC Session which is not managed by Slick. You can use this to wrap an
  * existing JDBC Connection. Override 'database' and 'performRollback'
  * as needed. */
@deprecated("Use the new Action-based API instead", "3.0")
class UnmanagedSession(val conn: Connection) extends JdbcBackend.SessionDef {
  protected var open = true
  protected var doRollback = false
  protected var inTransaction = false

  lazy val metaData = conn.getMetaData()
  lazy val capabilities = new JdbcBackend.DatabaseCapabilities(this)

  def close() = open = false

  def rollback() = doRollback = true

  def database: JdbcBackend.Database =
    throw new SlickException("No Database available for UnmanagedSession")

  def performRollback(): Unit =
    throw new SlickException("Cannot roll back UnmanagedSession")

  def withTransaction[T](f: => T): T = if(inTransaction) f else {
    inTransaction = true
    try {
      doRollback = false
      val res = f
      if(doRollback) performRollback()
      res
    } finally inTransaction = false
  }

  private[slick] def startInTransaction: Unit = ??
  private[slick] def endInTransaction(f: => Unit): Unit = ??
}

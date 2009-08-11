package com.novocode.squery.session

import java.sql.PreparedStatement
import com.novocode.squery.SQueryException

/**
 * A database session which opens a connection and transaction on demand.
 */
class Session private[squery] (db: Database) {

  var open = false
  var doRollback = false
  lazy val conn = { open = true; db.createConnection() }

  private[squery] def allocPS(sql: String) = conn.prepareStatement(sql)

  private[squery] def freePS(ps: PreparedStatement) = ps.close()

  private[squery] def withPS[T](sql: String)(f: (PreparedStatement => T)): T = {
    val st = allocPS(sql)
    try f(st) finally freePS(st)
  }

  def close() {
    if(open) conn.close()
  }

  /**
   * Call this method within a <em>withTransaction</em> call to roll back the current
   * transaction after <em>withTransaction</em> returns.
   */
  def rollback() {
    if(conn.getAutoCommit) throw new SQueryException("Cannot roll back session in auto-commit mode")
    doRollback = true
  }

  /**
   * Run the supplied function within a transaction. If the function throws an Exception
   * or the session's rollback() method is called, the transaction is rolled back,
   * otherwise it is commited when the function returns.
   */
  def withTransaction[T](f: => T): T = {
    conn.setAutoCommit(false)
    try {
      try {
        doRollback = false
        val res = f
        if(doRollback) conn.rollback()
        else conn.commit()
        res
      } catch {
        case ex:Exception =>
          conn.rollback()
          throw ex
      }
    } finally conn.setAutoCommit(true)
  }
}

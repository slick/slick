package com.novocode.squery.session

import java.sql.PreparedStatement
import com.novocode.squery.SQueryException

/**
 * A database session which opens a connection and transaction on demand.
 */
class Session private[session] (db: Database) extends java.io.Closeable {

  var open = false
  var doRollback = false
  lazy val conn = { open = true; db.createConnection() }

  def withPreparedStatement[T](sql: String)(f: (PreparedStatement => T)): T = {
    val st = conn.prepareStatement(sql)
    try f(st) finally st.close()
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
      var done = false
      try {
        doRollback = false
        val res = f
        if(doRollback) conn.rollback()
        else conn.commit()
        done = true
        res
      } finally if(!done) conn.rollback()
    } finally conn.setAutoCommit(true)
  }
}

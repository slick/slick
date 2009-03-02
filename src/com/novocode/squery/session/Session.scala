package com.novocode.squery.session

import scala.collection.mutable.{Map, Stack}
import java.sql.PreparedStatement

/**
 * A database session which opens a connection and transaction on demand.
 */
class Session private[squery] (fact: SessionFactory) {

  private class StatementMap {
    private val m = Map.empty[String, Stack[PreparedStatement]]
    def add(sql: String, ps: PreparedStatement) =  m get sql match {
      case Some(s) => s += ps
      case _ => {
        val s = new Stack[PreparedStatement]
        s += ps
        m += ((sql, s))
      }
    }
    def remove(sql: String) = m get sql match {
      case Some(s) => if(s.isEmpty) None else Some(s.pop)
      case _ => None
    }
    def foreach(f: PreparedStatement => Unit) =
      for(s <- m.values; ps <- s) f(ps);
  }

  private val freeStatements = new StatementMap
  private val usedStatements = new StatementMap

  var open = false
  var doRollback = false
  lazy val conn = { open = true; fact.createConnection() }

  private[squery] def allocPS(sql: String) = freeStatements remove sql match {
    case Some(ps) => { usedStatements add (sql, ps); ps }
    case _ => conn.prepareStatement(sql)
  }

  private[squery] def freePS(sql: String, ps: PreparedStatement) =
    if(!ps.isClosed) freeStatements add (sql, ps)

  def close() {
    freeStatements foreach (_.close)
    usedStatements foreach (_.close)
    if(open) conn.close
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

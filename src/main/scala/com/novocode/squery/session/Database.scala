package com.novocode.squery.session

import java.sql._
import javax.sql.DataSource
import scala.util.DynamicVariable

abstract class Database {

  protected[squery] def createConnection(): Connection

  def createSession(): Session = new Session(this)

  def withSession[T](f: Session => T): T = {
    val s = createSession()
    try { f(s) } finally s.close()
  }

  def withSession[T](f: => T): T = withSession { s: Session => Database.dyn.withValue(s)(f) }

  def withTransaction[T](f: Session => T): T = withSession { s => s.withTransaction(f(s)) }

  def withTransaction[T](f: => T): T = withSession { Database.getThreadSession.withTransaction(f) }
}

object Database {

  private val dyn = new DynamicVariable[Session](null)

  implicit def getThreadSession: Session = {
    val s = dyn.value
    if(s eq null)
      throw new SQLException("No implicit thread session available; getThreadSession() can only be used within a withSession block")
    else s
  }

  /**
   * Create a Database based on a DataSource.
   */
  def forDataSource(ds: DataSource): Database = new Database {
    protected[squery] def createConnection(): Connection = ds.getConnection
  }

  /**
   * Create a Database that uses the DriverManager to open new connections.
   */
  def forURL(url:String): Database = new Database {
    protected[squery] def createConnection(): Connection = DriverManager.getConnection(url)
  }
}

package com.novocode.squery.session

import java.sql._
import scala.util.DynamicVariable

abstract class SessionFactory {

  protected[squery] def createConnection(): Connection

  def createSession(): Session = new Session(this)

  def withSession[T](f: Session => T): T = {
    val s = createSession()
    try { f(s) } finally s.close()
  }

  def withSession[T](f: => T): T =
    withSession { s: Session => SessionFactory.dyn.withValue(s)(f) }

  def withTransaction[T](f: Session => T): T = withSession { s => s.withTransaction(f(s)) }

  def withTransaction[T](f: => T): T = withSession { SessionFactory.getThreadSession.withTransaction(f) }
}

object SessionFactory {

  private val dyn = new DynamicVariable[Session](null)

  implicit def getThreadSession: Session = {
    val s = dyn.value
    if(s eq null)
      throw new SQLException("No implicit thread session available; getThreadSession() can only be used within a withSession block")
    else s
  }
}

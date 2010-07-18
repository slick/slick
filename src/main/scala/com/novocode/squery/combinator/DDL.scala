package com.novocode.squery.combinator

import com.novocode.squery.session.Session

trait DDL { self =>
  /** Statements to execute first for create(), e.g. creating tables and indexes. */
  protected def createPhase1: Iterable[String]

  /** Statements to execute after createPhase1, e.g. creating foreign keys. */
  protected def createPhase2: Iterable[String]

  /** All statements to execute for create() */
  def createStatements: Iterator[String] = createPhase1.iterator ++ createPhase2.iterator

  /** Create the entities described by this DDL object */
  def create(implicit session: Session): Unit = session.withTransaction {
    for(s <- createStatements)
      session.withPreparedStatement(s)(_.execute)
  }

  def ++(other: DDL): DDL = new DDL {
    protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
    protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
  }
}

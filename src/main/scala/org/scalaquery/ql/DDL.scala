package org.scalaquery.ql

import org.scalaquery.session.Session

/**
 * A DDL object contains the SQL statements for creating and dropping
 * database entities. DDLs can be combined for creating or dropping multiple
 * entities together, even if they have circular dependencies.
 */
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

  /** Statements to execute first for drop(), e.g. removing connections from other entities. */
  protected def dropPhase1: Iterable[String]

  /** Statements to execute after dropPhase1, e.g. actually dropping a table. */
  protected def dropPhase2: Iterable[String]

  /** All statements to execute for drop() */
  def dropStatements: Iterator[String] = dropPhase1.iterator ++ dropPhase2.iterator

  /** Drop the entities described by this DDL object */
  def drop(implicit session: Session): Unit = session.withTransaction {
    for(s <- dropStatements)
      session.withPreparedStatement(s)(_.execute)
  }

  /** Create a new DDL object which combines this and the other DDL object. */
  def ++(other: DDL): DDL = new DDL {
    protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
    protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
    protected lazy val dropPhase1 = self.dropPhase1 ++ other.dropPhase1
    protected lazy val dropPhase2 = self.dropPhase2 ++ other.dropPhase2
  }
}

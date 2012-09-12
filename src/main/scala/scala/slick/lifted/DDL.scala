package scala.slick.lifted

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

  /** Statements to execute first for drop(), e.g. removing connections from other entities. */
  protected def dropPhase1: Iterable[String]

  /** Statements to execute after dropPhase1, e.g. actually dropping a table. */
  protected def dropPhase2: Iterable[String]

  /** All statements to execute for drop() */
  def dropStatements: Iterator[String] = dropPhase1.iterator ++ dropPhase2.iterator

  /** Create a new DDL object which combines this and the other DDL object. */
  def ++(other: DDL): DDL = new DDL {
    protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
    protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
    protected lazy val dropPhase1 = self.dropPhase1 ++ other.dropPhase1
    protected lazy val dropPhase2 = self.dropPhase2 ++ other.dropPhase2
  }
}

object DDL {
  def apply(create1: Iterable[String], create2: Iterable[String], drop1: Iterable[String],
            drop2: Iterable[String]): DDL = new DDL {
    protected def createPhase1 = create1
    protected def createPhase2 = create2
    protected def dropPhase1 = drop1
    protected def dropPhase2 = drop2
  }

  def apply(create1: Iterable[String], drop2: Iterable[String]): DDL = apply(create1, Nil, Nil, drop2)

  def apply(create1: String, drop2: String): DDL = apply(Iterable(create1), Iterable(drop2))
}

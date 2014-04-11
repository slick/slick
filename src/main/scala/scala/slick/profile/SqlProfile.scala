package scala.slick.profile

import scala.language.higherKinds
import scala.slick.ast.{TableNode, Symbol, SymbolNamer, Node, ColumnOption}
import scala.slick.lifted.AbstractTable

/** Basic profile for SQL-based drivers. */
trait SqlProfile extends RelationalProfile with SqlExecutorComponent with SqlTableComponent { driver: SqlDriver =>

  override protected def computeCapabilities = super.computeCapabilities ++ SqlProfile.capabilities.all

  type SchemaDescription = DDL

  trait DDL extends SchemaDescriptionDef { self =>
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

    /**
     * Create a new DDL object which combines this and the other DDL object.
     *
     * Composition is such that given {{{A.ddl ++ B.ddl}}} the create phases will be
     * run in FIFO order and the drop phases will be run in LIFO order.
     */
    def ++(other: DDL): DDL = new DDL {
      protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
      protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
      protected lazy val dropPhase1 = other.dropPhase1 ++ self.dropPhase1
      protected lazy val dropPhase2 = other.dropPhase2 ++ self.dropPhase2
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
}

object SqlProfile {
  object capabilities {
    /** Supports sequences (real or emulated) */
    val sequence = Capability("sql.sequence")
    /** Can get current sequence value */
    val sequenceCurr = Capability("sql.sequenceCurr")
    /** Supports cyclic sequences */
    val sequenceCycle = Capability("sql.sequenceCycle")
    /** Supports non-cyclic limited sequences (with a max value) */
    val sequenceLimited = Capability("sql.sequenceLimited")
    /** Supports max value for sequences */
    val sequenceMax = Capability("sql.sequenceMax")
    /** Supports min value for sequences */
    val sequenceMin = Capability("sql.sequenceMin")

    /** Supports all SqlProfile features which do not have separate capability values */
    val other = Capability("sql.other")

    /** All SQL capabilities */
    val all = Set(other, sequence, sequenceCurr, sequenceCycle,
      sequenceLimited, sequenceMax, sequenceMin)
  }
}

trait SqlDriver extends RelationalDriver with SqlProfile with SqlUtilsComponent {
  override val profile: SqlProfile = this
}

trait SqlUtilsComponent { driver: SqlDriver =>

  /** quotes identifiers to avoid collisions with SQL keywords and other syntax issues */
  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for(c <- id) if(c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def quoteTableName(t: TableNode): String = t.schemaName match {
    case Some(s) => quoteIdentifier(s) + "." + quoteIdentifier(t.tableName)
    case None => quoteIdentifier(t.tableName)
  }

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for(c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _ => b append c
    }
    b.toString
  }

  class QuotingSymbolNamer(parent: Option[SymbolNamer]) extends SymbolNamer("x", "y", parent) {
    override def namedSymbolName(s: Symbol) = quoteIdentifier(s.name)
  }
}

trait SqlExecutorComponent extends BasicExecutorComponent { driver: SqlDriver =>

  type QueryExecutor[T] <: QueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R]

  trait QueryExecutorDef[R] extends super.QueryExecutorDef[R] {
    def selectStatement: String
  }
}

trait SqlTableComponent extends RelationalTableComponent { driver: SqlDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions
}

package slick.profile

import slick.compiler.QueryCompiler

import scala.language.higherKinds
import slick.dbio._
import slick.ast.{TableNode, Symbol, SymbolNamer, Node, ColumnOption}
import slick.lifted.AbstractTable
import slick.util.DumpInfo

/** Basic profile for SQL-based drivers. */
trait SqlProfile extends RelationalProfile with SqlTableComponent with SqlActionComponent { driver: SqlDriver =>

  override protected def computeQueryCompiler = super.computeQueryCompiler ++ QueryCompiler.sqlPhases
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
    override def ++(other: DDL): DDL = new DDL {
      protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
      protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
      protected lazy val dropPhase1 = other.dropPhase1 ++ self.dropPhase1
      protected lazy val dropPhase2 = other.dropPhase2 ++ self.dropPhase2
    }

    override def hashCode() = 
      Vector(self.createPhase1, self.createPhase2, self.dropPhase1, self.dropPhase2).hashCode

    override def equals(o: Any) = o match {
      case ddl: DDL => 
        self.createPhase1 == ddl.createPhase1 &&
        self.createPhase2 == ddl.createPhase2 &&
        self.dropPhase1 == ddl.dropPhase1 &&
        self.dropPhase2 == ddl.dropPhase2
      case _ => false
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

  /** Extra column options for SqlProfile */
  object ColumnOption {
    case object NotNull extends ColumnOption[Nothing]
    case object Nullable extends ColumnOption[Nothing]

    /** Type as expected by the DBMS, e.g. VARCHAR or VARCHAR(254). Note that Slick's model omits
      * the optional length ascription for string columns here and carries the length in the
      * separate ColumnOption Length instead. A length ascription for string column is allowed
      * though and can be used in a Slick Table subclass to pass it to the DBMS. As this is the
      * type of the underlying DBMS it may not be portable to other DBMS.
      *
      * Note that Slick uses VARCHAR or VARCHAR(254) in DDL for String columns if neither
      * ColumnOption DBType nor Length are given. */
    case class SqlType(val typeName: String) extends ColumnOption[Nothing]
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

trait SqlTableComponent extends RelationalTableComponent { driver: SqlDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    def SqlType(typeName: String) = SqlProfile.ColumnOption.SqlType(typeName)
  }

  override val columnOptions: ColumnOptions = new ColumnOptions {}
}

trait SqlActionComponent extends RelationalActionComponent { driver: SqlDriver =>

  type DriverAction[+R, +S <: NoStream, -E <: Effect] <: SqlAction[R, S, E]
  type StreamingDriverAction[+R, +T, -E <: Effect] <: SqlStreamingAction[R, T, E] with DriverAction[R, Streaming[T], E]
}

trait SqlAction[+R, +S <: NoStream, -E <: Effect] extends BasicAction[R, S, E] {

  type ResultAction[+R, +S <: NoStream, -E <: Effect] <: SqlAction[R, S, E]

  /** Return the SQL statements that will be executed for this Action */
  def statements: Iterable[String]

  /** Create an Action that uses the specified SQL statement(s) but otherwise
    * behaves the same as this Action. */
  def overrideStatements(statements: Iterable[String]): ResultAction[R, S, E]

  def getDumpInfo = DumpInfo(DumpInfo.simpleNameFor(getClass), mainInfo = statements.mkString("[", "; ", "]"))
}

trait SqlStreamingAction[+R, +T, -E <: Effect] extends BasicStreamingAction[R, T, E] with SqlAction[R, Streaming[T], E]

trait FixedSqlAction[+R, +S <: NoStream, -E <: Effect] extends SqlAction[R, S, E] {
  type ResultAction[+R, +S <: NoStream, -E <: Effect] = SqlAction[R, S, E]
}

trait FixedSqlStreamingAction[+R, +T, -E <: Effect] extends SqlStreamingAction[R, T, E] with FixedSqlAction[R, Streaming[T], E]

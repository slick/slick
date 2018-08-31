package slick.sql

import slick.basic.{BasicStreamingAction, BasicAction}
import slick.compiler.QueryCompiler
import slick.relational.{RelationalActionComponent, RelationalTableComponent, RelationalProfile}

import scala.language.higherKinds
import slick.dbio._
import slick.ast.{TableNode, Symbol, SymbolNamer, ColumnOption}
import slick.util.DumpInfo

/** Abstract profile for SQL-based databases. */
trait SqlProfile extends RelationalProfile with SqlTableComponent with SqlActionComponent
  /* internal: */ with SqlUtilsComponent {

  @deprecated("Use the Profile object directly instead of calling `.profile` on it", "3.2")
  override val profile: SqlProfile = this

  override protected def computeQueryCompiler = super.computeQueryCompiler ++ QueryCompiler.sqlPhases
  override protected def computeCapabilities = super.computeCapabilities ++ SqlCapabilities.all

  type SchemaDescription = DDL

  trait DDL extends SchemaDescriptionDef { self =>
    /** Statements to execute first for create(), e.g. creating tables and indexes. */
    protected def createPhase1: Iterable[String]

    /** Statements to execute after createPhase1, e.g. creating foreign keys. */
    protected def createPhase2: Iterable[String]

    protected def createIfNotExistsPhase: Iterable[String]

    /** All statements to execute for create() */
    def createStatements: Iterator[String] = createPhase1.iterator ++ createPhase2.iterator

    /** All statements to execute for createIfNotExists() */
    def createIfNotExistsStatements: Iterator[String] = createIfNotExistsPhase.iterator

    /** Statements to execute first for drop(), e.g. removing connections from other entities. */
    protected def dropPhase1: Iterable[String]

    protected def dropIfExistsPhase: Iterable[String]

    /** Statements to execute after dropPhase1, e.g. actually dropping a table. */
    protected def dropPhase2: Iterable[String]

    /** All statements to execute for drop() */
    def dropStatements: Iterator[String] = dropPhase1.iterator ++ dropPhase2.iterator

    /** All statements to execute for dropIfExists() */
    def dropIfExistsStatements: Iterator[String] = dropIfExistsPhase.iterator

    /** Statements to execute first for truncate() */
    protected def truncatePhase: Iterable[String]
    /** All statements to execute for truncate */
    def truncateStatements: Iterator[String] = truncatePhase.iterator

    /**
     * Create a new DDL object which combines this and the other DDL object.
     *
     * Composition is such that given {{{A.ddl ++ B.ddl}}} the create phases will be
     * run in FIFO order and the drop phases will be run in LIFO order.
     */
    override def ++(other: DDL): DDL = new DDL {
      protected lazy val createPhase1 = self.createPhase1 ++ other.createPhase1
      protected lazy val createPhase2 = self.createPhase2 ++ other.createPhase2
      protected lazy val createIfNotExistsPhase = self.createIfNotExistsPhase ++ other.createIfNotExistsPhase
      protected lazy val dropPhase1 = other.dropPhase1 ++ self.dropPhase1
      protected lazy val dropIfExistsPhase = other.dropIfExistsPhase ++ self.dropIfExistsPhase
      protected lazy val dropPhase2 = other.dropPhase2 ++ self.dropPhase2
      protected lazy val truncatePhase = other.truncatePhase ++ self.truncatePhase
    }

    override def hashCode() = 
      Vector(self.createPhase1, self.createPhase2, self.dropPhase1, self.dropPhase2 , self.truncatePhase).hashCode

    override def equals(o: Any) = o match {
      case ddl: DDL => 
        self.createPhase1 == ddl.createPhase1 &&
        self.createIfNotExistsPhase == ddl.createIfNotExistsPhase &&
        self.createPhase2 == ddl.createPhase2 &&
        self.dropPhase1 == ddl.dropPhase1 &&
        self.dropIfExistsPhase == ddl.dropIfExistsPhase &&
        self.dropPhase2 == ddl.dropPhase2 &&
        self.truncatePhase == ddl.truncatePhase 
      case _ => false
    }
  }

  object DDL { 
    def apply(create1: Iterable[String], createIfNotExists: Iterable[String], create2: Iterable[String], drop1: Iterable[String],
              dropIfExists: Iterable[String], drop2: Iterable[String] , truncate: Iterable[String]): DDL = new DDL {
      protected def createPhase1 = create1
      protected def createIfNotExistsPhase = createIfNotExists
      protected def createPhase2 = create2
      protected def dropPhase1 = drop1
      protected def dropIfExistsPhase = dropIfExists
      protected def dropPhase2 = drop2
      protected def truncatePhase = truncate
    }

    def apply(create1: Iterable[String], drop2: Iterable[String]): DDL = apply(create1, Nil, Nil, Nil, Nil, drop2 , Nil)

    def apply(create1: String, drop2: String): DDL = apply(Iterable(create1), Iterable(drop2))
  }
}

object SqlProfile {
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

trait SqlUtilsComponent { self: SqlProfile =>

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

trait SqlTableComponent extends RelationalTableComponent { this: SqlProfile =>

  trait ColumnOptions extends super.ColumnOptions {
    def SqlType(typeName: String) = SqlProfile.ColumnOption.SqlType(typeName)
  }

  override val columnOptions: ColumnOptions = new ColumnOptions {}
}

trait SqlActionComponent extends RelationalActionComponent { this: SqlProfile =>

  type ProfileAction[+R, +S <: NoStream, -E <: Effect] <: SqlAction[R, S, E]
  type StreamingProfileAction[+R, +T, -E <: Effect] <: SqlStreamingAction[R, T, E] with ProfileAction[R, Streaming[T], E]
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

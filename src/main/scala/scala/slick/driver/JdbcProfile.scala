package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.{Node, TypedType, BaseTypedType}
import scala.slick.compiler.QueryCompiler
import scala.slick.lifted._
import scala.slick.jdbc.{CompileInsert, JdbcCodeGen, JdbcBackend, JdbcType, MappedJdbcType}
import scala.slick.profile.{SqlDriver, SqlProfile, Capability}

/**
 * A profile for accessing SQL databases via JDBC.
 */
trait JdbcProfile extends SqlProfile with JdbcTableComponent
  with JdbcInvokerComponent with JdbcExecutorComponent { driver: JdbcDriver =>

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  val compiler = QueryCompiler.relational
  val Implicit = new Implicits
  val columnTypes = new JdbcTypes

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcProfile.capabilities.all

  lazy val selectStatementCompiler = compiler + new JdbcCodeGen[this.type](this)(_.buildSelect)
  lazy val updateStatementCompiler = compiler + new JdbcCodeGen[this.type](this)(_.buildUpdate)
  lazy val deleteStatementCompiler = compiler + new JdbcCodeGen[this.type](this)(_.buildDelete)
  lazy val insertStatementCompiler = QueryCompiler(new CompileInsert(this))

  final def buildTableDDL(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceDDL(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  def queryToQueryTemplate[P,R](q: Query[_, R]): QueryTemplate[P,R] =
    createQueryTemplate[P,R](selectStatementCompiler.run(Node(q)).tree)

  class Implicits extends ImplicitJdbcTypes with ExtensionMethodConversions {
    implicit val slickDriver: driver.type = driver
    implicit def columnToOptionColumn[T : BaseTypedType](c: Column[T]): Column[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypedType](v: T) = new ConstColumn[T](v)
    implicit def tableToQuery[T <: AbstractTable[_]](t: T) = Query[T, NothingContainer#TableNothing, T](t)(Shape.tableShape)
    implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc
    implicit def ddlToDDLInvoker(d: DDL): DDLInvoker = new DDLInvoker(d)
    implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): QueryInvoker[U] = createQueryInvoker[U](selectStatementCompiler.run(Node(q)).tree)
    implicit def queryToDeleteInvoker(q: Query[_ <: Table[_], _]): DeleteInvoker = new DeleteInvoker(deleteStatementCompiler.run(Node(q)).tree)
    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = createCountingInsertInvoker[T](insertStatementCompiler.run(Node(c)).tree)
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = createCountingInsertInvoker[U](insertStatementCompiler.run(u.packedNode).tree)

    implicit def queryToQueryExecutor[E, U](q: Query[E, U]): QueryExecutor[Seq[U]] = createQueryExecutor[Seq[U]](selectStatementCompiler.run(Node(q)).tree)

    // We can't use this direct way due to SI-3346
    def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[M, R, _]): QueryExecutor[R] = createQueryExecutor[R](selectStatementCompiler.run(Node(q)).tree)
    implicit final def recordToUnshapedQueryExecutor[M <: Rep[_]](q: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutor[M](q)
    implicit final def anyToToQueryExecutor[T](value: T) = new ToQueryExecutor[T](value)

    // We should really constrain the 2nd type parameter of Query but that won't
    // work for queries on implicitly lifted tables. This conversion is needed
    // for mapped tables.
    implicit def tableQueryToUpdateInvoker[T](q: Query[_ <: Table[T], NothingContainer#TableNothing]): UpdateInvoker[T] =
      createUpdateInvoker(updateStatementCompiler.run(Node(q)).tree)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): UpdateInvoker[T] =
      createUpdateInvoker(updateStatementCompiler.run(Node(q)).tree)

    // Work-around for SI-3346
    @inline implicit final def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }

  trait SimpleQL extends Implicits with scala.slick.lifted.Aliases {
    type Table[T] = driver.Table[T]
    type Database = Backend#Database
    val Database = backend.Database
    type Session = Backend#Session
    type SlickException = scala.slick.SlickException
    type ColumnType[T] = JdbcType[T]
    type BaseColumnType[T] = JdbcType[T] with BaseTypedType[T]
    type MappedColumnType[T, U] = MappedJdbcType[T, U]
    val MappedColumnType = MappedJdbcType
  }

  /** A collection of values for using the query language with a single import
    * statement. This provides the driver's implicits, the Database and
    * Session objects for DB connections, and commonly used query language
    * types and objects. */
  val simple: SimpleQL = new SimpleQL {}
}

object JdbcProfile {
  object capabilities {
    /** Supports mutable result sets */
    val mutable = Capability("jdbc.mutable")
    /** Can return primary key of inserted row */
    val returnInsertKey = Capability("jdbc.returnInsertKey")
    /** Can also return non-primary-key columns of inserted row */
    val returnInsertOther = Capability("jdbc.returnInsertOther")

    /** Supports all JdbcProfile features which do not have separate capability values */
    val other = Capability("jdbc.other")

    /** All JDBC capabilities */
    val all = Set(other, mutable, returnInsertKey, returnInsertOther)
  }
}

trait JdbcDriver extends SqlDriver
  with JdbcProfile
  with JdbcStatementBuilderComponent
  with JdbcTypesComponent { driver =>

  override val profile: JdbcProfile = this

  def quote[T](v: T)(implicit tm: TypedType[T]): String = typeInfoFor(tm).valueToSQLLiteral(v)
}

object JdbcDriver extends JdbcDriver

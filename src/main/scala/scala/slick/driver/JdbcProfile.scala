package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.Node
import scala.slick.compiler.QueryCompiler
import scala.slick.lifted._
import scala.slick.jdbc.JdbcBackend
import scala.slick.profile.{SqlProfile, Capability}

/**
 * A profile for accessing SQL databases via JDBC.
 */
trait JdbcProfile extends SqlProfile with JdbcTableComponent
  with JdbcInvokerComponent with JdbcExecutorComponent { driver: JdbcDriver =>

  type Backend = JdbcBackend
  val backend: Backend = JdbcBackend
  val compiler = QueryCompiler.relational
  val Implicit = new Implicits
  val typeMapperDelegates = new TypeMapperDelegates

  override protected def computeCapabilities = super.computeCapabilities ++ JdbcProfile.capabilities.all

  final def createQueryBuilder(q: Query[_, _]): QueryBuilder = createQueryBuilder(new QueryBuilderInput(compiler.run(Node(q)), q))
  final def buildSelectStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildSelect
  final def buildUpdateStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildUpdate
  final def buildDeleteStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildDelete
  @deprecated("Use createInsertBuilder.buildInsert", "1.0")
  final def buildInsertStatement(cb: Any): InsertBuilderResult = createInsertBuilder(Node(cb)).buildInsert
  @deprecated("Use createInsertBuilder.buildInsert", "1.0")
  final def buildInsertStatement(cb: Any, q: Query[_, _]): InsertBuilderResult = createInsertBuilder(Node(cb)).buildInsert(q)
  final def buildTableDDL(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceDDL(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  class Implicits extends ExtensionMethodConversions {
    implicit val slickDriver: driver.type = driver
    implicit def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)
    implicit def tableToQuery[T <: AbstractTable[_]](t: T) = Query[T, NothingContainer#TableNothing, T](t)(Shape.tableShape)
    implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc
    implicit def ddlToDDLInvoker(d: DDL): DDLInvoker = new DDLInvoker(d)
    implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): QueryInvoker[T, U] = new QueryInvoker(q)
    implicit def queryToDeleteInvoker(q: Query[_ <: Table[_], _]): DeleteInvoker = new DeleteInvoker(q)
    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = createCountingInsertInvoker(ShapedValue.createShapedValue(c))
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = createCountingInsertInvoker(u)

    implicit def queryToQueryExecutor[E, U](q: Query[E, U]): QueryExecutor[Seq[U]] = new QueryExecutor[Seq[U]](new QueryBuilderInput(compiler.run(Node(q)), q))

    // We can't use this direct way due to SI-3346
    def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[M, R, _]): QueryExecutor[R] = new QueryExecutor[R](new QueryBuilderInput(compiler.run(Node(q)), shape.linearizer(q)))
    implicit final def recordToUnshapedQueryExecutor[M <: Rep[_]](q: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutor[M](q)
    implicit final def anyToToQueryExecutor[T](value: T) = new ToQueryExecutor[T](value)

    // We should really constrain the 2nd type parameter of Query but that won't
    // work for queries on implicitly lifted tables. This conversion is needed
    // for mapped tables.
    implicit def tableQueryToUpdateInvoker[T](q: Query[_ <: Table[T], NothingContainer#TableNothing]): UpdateInvoker[T] = new UpdateInvoker(q.asInstanceOf[Query[Table[T], T]])

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): UpdateInvoker[T] = new UpdateInvoker(q)

    // Work-around for SI-3346
    @inline implicit final def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }

  trait SimpleQL extends Implicits with scala.slick.lifted.Aliases {
    type Table[T] = driver.Table[T]
    type Database = Backend#Database
    val Database = backend.Database
    type Session = Backend#Session
    type SlickException = scala.slick.SlickException
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

trait JdbcDriver extends JdbcProfile
  with JdbcStatementBuilderComponent
  with JdbcTypeMapperDelegatesComponent
  with JdbcSQLUtilsComponent {
  val profile: JdbcProfile = this
}

object JdbcDriver extends JdbcDriver

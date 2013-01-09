package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.Node
import scala.slick.compiler.QueryCompiler
import scala.slick.lifted._

trait BasicProfile extends BasicTableComponent { driver: BasicDriver =>

  val compiler = QueryCompiler.relational
  val Implicit = new Implicits
  val typeMapperDelegates = new TypeMapperDelegates
  val capabilities: Set[Capability] = BasicProfile.capabilities.all

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
    type Database = scala.slick.session.Database
    val Database = scala.slick.session.Database
    type Session = scala.slick.session.Session
    type SlickException = scala.slick.SlickException
  }

  /** A collection of values for using the query language with a single import
    * statement. This provides the driver's implicits, the Database and
    * Session objects for DB connections, and commonly used query language
    * types and objects. */
  val simple: SimpleQL = new SimpleQL {}
}

object BasicProfile {
  object capabilities {
    /** Supports default values in column definitions */
    val columnDefaults = Capability("basic.columnDefaults")
    /** Supports foreignKeyActions */
    val foreignKeyActions = Capability("basic.foreignKeyActions")
    /** Supports the ''database'' function to get the current database name.
      * A driver without this capability will return an empty string. */
    val functionDatabase = Capability("basic.functionDatabase")
    /** Supports the ''user'' function to get the current database user.
      * A driver without this capability will return an empty string. */
    val functionUser = Capability("basic.functionUser")
    /** Supports full outer joins */
    val joinFull = Capability("basic.joinFull")
    /** Supports right outer joins */
    val joinRight = Capability("basic.joinRight")
    /** Supports escape characters in "like" */
    val likeEscape = Capability("basic.likeEscape")
    /** Supports mutable result sets */
    val mutable = Capability("basic.mutable")
    /** Supports .drop on queries */
    val pagingDrop = Capability("basic.pagingDrop")
    /** Supports properly compositional paging in sub-queries */
    val pagingNested = Capability("basic.pagingNested")
    /** Returns only the requested number of rows even if some rows are not
      * unique. Without this capability, non-unique rows may be counted as
      * only one row each. */
    val pagingPreciseTake = Capability("basic.pagingPreciseTake")
    /** Can return primary key of inserted row */
    val returnInsertKey = Capability("basic.returnInsertKey")
    /** Can also return non-primary-key columns of inserted row */
    val returnInsertOther = Capability("basic.returnInsertOther")
    /** Supports sequences (real or emulated) */
    val sequence = Capability("basic.sequence")
    /** Can get current sequence value */
    val sequenceCurr = Capability("basic.sequenceCurr")
    /** Supports cyclic sequences */
    val sequenceCycle = Capability("basic.sequenceCycle")
    /** Supports non-cyclic limited sequences (with a max value) */
    val sequenceLimited = Capability("basic.sequenceLimited")
    /** Supports max value for sequences */
    val sequenceMax = Capability("basic.sequenceMax")
    /** Supports min value for sequences */
    val sequenceMin = Capability("basic.sequenceMin")
    /** Can set an Option[ Array[Byte] ] column to None */
    val setByteArrayNull = Capability("basic.setByteArrayNull")
    /** Supports the BigDecimal data type */
    val typeBigDecimal = Capability("basic.typeBigDecimal")
    /** Supports the Blob data type */
    val typeBlob = Capability("basic.typeBlob")
    /** Supports the Long data type */
    val typeLong = Capability("basic.typeLong")
    /** Supports zip, zipWith and zipWithIndex */
    val zip = Capability("basic.zip")

    /** Supports all BasicProfile features which do not have separate capability values */
    val basic = Capability("basic")

    /** All basic capabilities */
    val all = Set(basic, columnDefaults, foreignKeyActions, functionDatabase,
      functionUser, joinFull, joinRight, likeEscape, mutable, pagingDrop,
      pagingNested, pagingPreciseTake, returnInsertKey, returnInsertOther,
      sequence, sequenceCurr, sequenceCycle, sequenceLimited, sequenceMax,
      sequenceMin, setByteArrayNull, typeBigDecimal, typeBlob, typeLong, zip)
  }
}

trait BasicDriver extends BasicProfile
  with BasicStatementBuilderComponent
  with BasicTypeMapperDelegatesComponent
  with BasicSQLUtilsComponent
  with BasicExecutorComponent
  with BasicInvokerComponent {
  val profile: BasicProfile = this
}

object BasicDriver extends BasicDriver

package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.{FieldSymbol, Node}
import scala.slick.compiler.QueryCompiler
import scala.slick.lifted._

trait BasicProfile extends BasicTableComponent { driver: BasicDriver =>

  // Create the different builders -- these methods should be overridden by drivers as needed
  def createQueryTemplate[P,R](q: Query[_, R]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](q, this)
  def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)
  def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder = new SequenceDDLBuilder(seq)

  val compiler = QueryCompiler.relational
  val Implicit = new Implicits
  val typeMapperDelegates = new TypeMapperDelegates

  final def createQueryBuilder(q: Query[_, _]): QueryBuilder = createQueryBuilder(new QueryBuilderInput(compiler.run(Node(q)), q))
  final def buildSelectStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildSelect
  final def buildUpdateStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildUpdate
  final def buildDeleteStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildDelete
  final def buildInsertStatement(cb: Any): String = new InsertBuilder(cb).buildInsert
  final def buildInsertStatement(cb: Any, q: Query[_, _]): QueryBuilderResult = new InsertBuilder(cb).buildInsert(q)
  final def buildTableDDL(table: Table[_]): DDL = createTableDDLBuilder(table).buildDDL
  final def buildSequenceDDL(seq: Sequence[_]): DDL = createSequenceDDLBuilder(seq).buildDDL

  class Implicits extends ExtensionMethodConversions {
    implicit val slickDriver: driver.type = driver
    implicit def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)
    implicit def tableToQuery[T <: AbstractTable[_]](t: T) = Query[T, NothingContainer#TableNothing, T](t)(Shape.tableShape)
    implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc
    implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): QueryInvoker[T, U] = new QueryInvoker(q)
    implicit def queryToDeleteInvoker(q: Query[_ <: Table[_], _]): DeleteInvoker = new DeleteInvoker(q)
    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new InsertInvoker(ShapedValue.createShapedValue(c))
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = new InsertInvoker(u)

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

  class SimpleQL extends Implicits with scala.slick.lifted.Aliases {
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
  val simple = new SimpleQL
}

trait BasicDriver extends BasicProfile
  with BasicStatementBuilderComponent
  with BasicTypeMapperDelegatesComponent
  with BasicSQLUtilsComponent
  with BasicExecutorComponent
  with BasicInvokerComponent{
  val profile: BasicProfile = this
}

object BasicDriver extends BasicDriver

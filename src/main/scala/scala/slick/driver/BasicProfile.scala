package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.ast.{Optimizer, NodeGenerator, Node, Relational}
import scala.slick.ql._
import slick.util.ValueLinearizer

trait BasicProfile extends BasicTableComponent { driver: BasicDriver =>

  def createQueryTemplate[P,R](q: Query[_, R]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](q, this)
  def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)
  final def createQueryBuilder(q: Query[_, _]): QueryBuilder = createQueryBuilder(processAST(q), q)

  val Implicit = new Implicits
  val typeMapperDelegates = new TypeMapperDelegates

  def buildSelectStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildSelect
  def buildUpdateStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildUpdate
  def buildDeleteStatement(q: Query[_, _]): QueryBuilderResult = createQueryBuilder(q).buildDelete

  def buildInsertStatement(cb: Any): String = new InsertBuilder(cb).buildInsert
  def buildInsertStatement(cb: Any, q: Query[_, _]): QueryBuilderResult = new InsertBuilder(cb).buildInsert(q)

  def buildTableDDL(table: Table[_]): DDL = new DDLBuilder(table).buildDDL
  def buildSequenceDDL(seq: Sequence[_]): DDL = new SequenceDDLBuilder(seq).buildDDL

  def processAST(g: NodeGenerator): Node = Relational(Optimizer(Node(g)))

  class Implicits {
    implicit val slickDriver: driver.type = driver
    implicit def baseColumnToColumnOps[B1 : BaseTypeMapper](c: Column[B1]): ColumnOps[B1, B1] =
      new ColumnOps[B1, B1](Node(c))
    implicit def optionColumnToColumnOps[B1](c: Column[Option[B1]]): ColumnOps[B1, Option[B1]] =
      new ColumnOps[B1, Option[B1]](Node(c))
    implicit def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)
    implicit def tableToQuery[T <: AbstractTable[_]](t: T) = Query[T, TableNothing, T](t)(Shape.tableShape)
    implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc
    implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): QueryInvoker[T, U] = new QueryInvoker(q)
    implicit def queryToDeleteInvoker(q: Query[_ <: Table[_], _]): DeleteInvoker = new DeleteInvoker(q)
    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new InsertInvoker(ShapedValue.createShapedValue(c))
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = new InsertInvoker(u)

    implicit def queryToQueryExecutor[E, U](q: Query[E, U]): QueryExecutor[Seq[U]] = new QueryExecutor[Seq[U]](processAST(Node(q)), driver, q)

    // We can't use this direct way due to SI-3346
    //implicit def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[M, R, _]): QueryExecutor[R] = new QueryExecutor[R](processAST(Node(q)), driver, shape.linearizer(q))
    implicit def recordToQueryExecutor[M <: Rep[_]](q: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutor[M](q, processAST(Node(q)), driver)

    // We should really constrain the 2nd type parameter of Query but that won't
    // work for queries on implicitly lifted tables. This conversion is needed
    // for mapped tables.
    implicit def tableQueryToUpdateInvoker[T](q: Query[_ <: Table[T], _]): UpdateInvoker[T] = new UpdateInvoker(q.asInstanceOf[Query[Table[T], T]])

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): UpdateInvoker[T] = new UpdateInvoker(q)

    // Work-around for SI-3346
    @inline implicit final def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }
}

trait BasicDriver extends BasicProfile
  with BasicStatementBuilderComponent
  with BasicTypeMapperDelegatesComponent
  with BasicSQLUtilsComponent
  with BasicInvokerComponent

object BasicDriver extends BasicDriver

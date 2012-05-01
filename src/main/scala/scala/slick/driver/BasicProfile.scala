package scala.slick.driver

import scala.slick.ast.{Optimizer, NodeGenerator, Node, Relational}
import scala.slick.ql._
import slick.util.ValueLinearizer

trait BasicProfile { driver: BasicDriver =>

  def createQueryTemplate[P,R](query: Query[_, R]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)
  final def createQueryBuilder(query: Query[_, _]): QueryBuilder = createQueryBuilder(processAST(query), query)

  val Implicit = new Implicits
  val typeMapperDelegates = new TypeMapperDelegates

  def buildSelectStatement(query: Query[_, _]): QueryBuilderResult =
    createQueryBuilder(query).buildSelect
  def buildUpdateStatement(query: Query[_, _]): QueryBuilderResult =
    createQueryBuilder(query).buildUpdate
  def buildDeleteStatement(query: Query[_, _]): QueryBuilderResult =
    createQueryBuilder(query).buildDelete

  def buildInsertStatement(cb: Any): String = new InsertBuilder(cb).buildInsert
  def buildInsertStatement(cb: Any, q: Query[_, _]): QueryBuilderResult =
    new InsertBuilder(cb).buildInsert(q)

  def buildTableDDL(table: AbstractBasicTable[_]): DDL = new DDLBuilder(table).buildDDL
  def buildSequenceDDL(seq: Sequence[_]): DDL = new SequenceDDLBuilder(seq).buildDDL

  def processAST(g: NodeGenerator): Node = Relational(Optimizer(Node(g)))

  class Implicits {
    implicit val slickDriver: driver.type = driver
    implicit def baseColumnToColumnOps[B1 : BaseTypeMapper](c: Column[B1]): ColumnOps[B1, B1] = c match {
      case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, B1]]
      case _ => new ColumnOps[B1, B1] { protected[this] val leftOperand = Node(c) }
    }
    implicit def optionColumnToColumnOps[B1](c: Column[Option[B1]]): ColumnOps[B1, Option[B1]] = c match {
      case o: ColumnOps[_,_] => o.asInstanceOf[ColumnOps[B1, Option[B1]]]
      case _ => new ColumnOps[B1, Option[B1]] { protected[this] val leftOperand = Node(c) }
    }
    implicit def columnToOptionColumn[T : BaseTypeMapper](c: Column[T]): Column[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypeMapper](v: T) = new ConstColumn[T](v)
    implicit def tableToQuery[T <: AbstractTable[_]](t: T) = Query[T, TableNothing, T](t)(Shape.tableShape)
    implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc
    implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): BasicQueryInvoker[T, U] = new BasicQueryInvoker(q, driver)
    implicit def queryToDeleteInvoker(q: Query[_ <: AbstractBasicTable[_], _]): BasicDeleteInvoker = new BasicDeleteInvoker(q, driver)
    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new BasicInsertInvoker(ShapedValue.createShapedValue(c), driver)
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = new BasicInsertInvoker(u, driver)

    implicit def queryToQueryExecutor[E, U](q: Query[E, U]): QueryExecutor[Seq[U]] = new QueryExecutor[Seq[U]](processAST(Node(q)), driver, q)

    // We can't use this direct way due to SI-3346
    //implicit def recordToQueryExecutor[M, R](q: M)(implicit shape: Shape[M, R, _]): QueryExecutor[R] = new QueryExecutor[R](processAST(Node(q)), driver, shape.linearizer(q))
    implicit def recordToQueryExecutor[M <: Rep[_]](q: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutor[M](q, processAST(Node(q)), driver)

    // We should really constrain the 2nd type parameter of Query but that won't
    // work for queries on implicitly lifted tables. This conversion is needed
    // for mapped tables.
    implicit def tableQueryToUpdateInvoker[T](q: Query[_ <: AbstractBasicTable[T], _]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q.asInstanceOf[Query[AbstractBasicTable[T], T]], driver)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q, driver)

    // Work-around for SI-3346
    implicit def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }
}

trait BasicDriver extends BasicProfile
  with BasicStatementBuilderComponent with BasicTypeMapperDelegatesComponent with BasicSQLUtilsComponent

object BasicDriver extends BasicDriver

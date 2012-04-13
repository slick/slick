package org.scalaquery.ql.basic

import org.scalaquery.ast.{Optimizer, NodeGenerator, Node, Relational}
import org.scalaquery.ql._
import org.scalaquery.{ToShapedValue, ShapedValue, Shape}

trait BasicProfile { driver: BasicDriver =>

  def createQueryTemplate[P,R](query: Query[_, R]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_, _]): QueryBuilder = new QueryBuilder(processAST(query), query)

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
    implicit val scalaQueryDriver: driver.type = driver
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
    implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): BasicQueryInvoker[T, U] = new BasicQueryInvoker(q, scalaQueryDriver)
    implicit def queryToDeleteInvoker(q: Query[_ <: AbstractBasicTable[_], _]): BasicDeleteInvoker = new BasicDeleteInvoker(q, scalaQueryDriver)
    implicit def columnBaseToInsertInvoker[T](c: ColumnBase[T]) = new BasicInsertInvoker(ShapedValue.createShapedValue(c), scalaQueryDriver)
    implicit def shapedValueToInsertInvoker[T, U](u: ShapedValue[T, U]) = new BasicInsertInvoker(u, scalaQueryDriver)

    // We should really constrain the 2nd type parameter of Query but that won't
    // work for queries on implicitly lifted tables. This conversion is needed
    // for mapped tables.
    implicit def tableQueryToUpdateInvoker[T](q: Query[_ <: AbstractBasicTable[T], _]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q.asInstanceOf[Query[AbstractBasicTable[T], T]], scalaQueryDriver)

    // This conversion only works for fully packed types
    implicit def productQueryToUpdateInvoker[T](q: Query[_ <: ColumnBase[T], T]): BasicUpdateInvoker[T] = new BasicUpdateInvoker(q, scalaQueryDriver)

    // Work-around for SI-3346
    implicit def anyToToShapedValue[T](value: T) = new ToShapedValue[T](value)
  }
}

trait BasicDriver extends BasicProfile
  with BasicStatementBuilderComponent with BasicTypeMapperDelegatesComponent with BasicSQLUtilsComponent

object BasicDriver extends BasicDriver

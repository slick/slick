package scala.slick.driver

import scala.slick.ast.{CompiledStatement, First, ResultSetMapping, Node}
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.util.SQLBuilder
import scala.slick.profile.SqlExecutorComponent
import scala.slick.lifted.{Shape, ShapeLevel}

trait JdbcExecutorComponent extends SqlExecutorComponent { driver: JdbcDriver =>

  type QueryExecutor[T] = QueryExecutorDef[T]
  type UnshapedQueryExecutor[T] = UnshapedQueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)
  def createUnshapedQueryExecutor[M](value: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutorDef[M](value)

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {

    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql

    def run(implicit session: Backend#Session): R = (tree match {
      case rsm: ResultSetMapping =>
        createQueryInvoker[Any](rsm, param).buildColl(session, rsm.nodeType.asCollectionType.cons.canBuildFrom)
      case First(rsm: ResultSetMapping) =>
        createQueryInvoker[Any](rsm, param).first
    }).asInstanceOf[R]
  }

  class UnshapedQueryExecutorDef[M](value: M) extends super.UnshapedQueryExecutorDef[M](value) {
    @inline final def selectStatement[U](implicit shape: Shape[_ <: ShapeLevel.Flat, M, U, _], session: Backend#Session): String =
      executor[U].selectStatement
  }
}

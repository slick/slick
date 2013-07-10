package scala.slick.driver

import scala.slick.ast.{CompiledStatement, First, ResultSetMapping, Node}
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.util.SQLBuilder
import scala.slick.profile.SqlExecutorComponent

trait JdbcExecutorComponent extends SqlExecutorComponent { driver: JdbcDriver =>

  type QueryExecutor[T] = QueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {

    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql

    def run(implicit session: Backend#Session): R = (tree match {
      case rsm: ResultSetMapping =>
        createQueryInvoker[Any, Any](rsm).to(param)(session, rsm.nodeType.asCollectionType.cons.canBuildFrom)
      case First(rsm: ResultSetMapping) =>
        createQueryInvoker[Any, Any](rsm).first(param)
    }).asInstanceOf[R]
  }
}

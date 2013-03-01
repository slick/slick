package scala.slick.driver

import scala.slick.ast.{CompiledStatement, First, ResultSetMapping, Node}
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.lifted.Shape
import scala.slick.util.SQLBuilder

trait JdbcExecutorComponent { driver: JdbcDriver =>

  // Create an executor -- this method should be overridden by drivers as needed
  def createQueryExecutor[R](tree: Node, param: Any) = new QueryExecutor[R](tree, param)

  class QueryExecutor[R](tree: Node, param: Any) {

    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql

    def run(implicit session: Backend#Session): R = (tree match {
      case rsm: ResultSetMapping =>
        createQueryInvoker[Any, Any](rsm).to(param, session, rsm.nodeType.asCollectionType.cons.canBuildFrom)
      case First(rsm: ResultSetMapping) =>
        createQueryInvoker[Any, Any](rsm).first(param)
    }).asInstanceOf[R]

    def executor: this.type = this
  }

  // Work-around for SI-3346
  final class UnshapedQueryExecutor[M](val value: M) {
    @inline def run[U](implicit shape: Shape[M, U, _], session: Backend#Session): U =
      Implicit.recordToQueryExecutor(value).run
  }

  // Work-around for SI-3346
  final class ToQueryExecutor[M](val value: M) {
    @inline def toQueryExecutor[U](implicit shape: Shape[M, U, _]): QueryExecutor[U] =
      Implicit.recordToQueryExecutor(value)
  }
}

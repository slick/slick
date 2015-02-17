package slick.driver

import scala.collection.mutable.Builder
import slick.ast._
import slick.ast.Util._
import slick.ast.TypeUtil._
import slick.util.SQLBuilder
import slick.profile.SqlExecutorComponent
import slick.lifted.{Shape, FlatShapeLevel}
import slick.relational.CompiledMapping

/** The part of the driver cake that handles the <em>executor</em> API for
  * running queries. */
trait JdbcExecutorComponent extends SqlExecutorComponent { driver: JdbcDriver =>

  type QueryExecutor[T] = QueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {

    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql

    def run(implicit session: Backend#Session): R = tree match {
      case rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType)) :@ CollectionType(cons, el) =>
        val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, R]]
        createQueryInvoker[Any](rsm, param, null).foreach({ x => b += x }, 0)(session)
        b.result()
      case First(rsm: ResultSetMapping) =>
        createQueryInvoker[R](rsm, param, null).first
    }
  }
}

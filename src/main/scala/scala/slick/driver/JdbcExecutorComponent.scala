package scala.slick.driver

import scala.collection.mutable.Builder
import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.util.SQLBuilder
import scala.slick.profile.SqlExecutorComponent
import scala.slick.lifted.{Shape, FlatShapeLevel}
import scala.slick.relational.CompiledMapping

/** The part of the driver cake that handles the <em>executor</em> API for
  * running queries. */
trait JdbcExecutorComponent extends SqlExecutorComponent { driver: JdbcDriver =>

  type QueryExecutor[T] = QueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new QueryExecutorDef[R](tree, param)

  class QueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {
    /** Replace the SQL query used by this invoker.
      * This can be used as a workaround to replace
      * non-optimal SQL produced by Slick's query compiler.
      */
    def hackSql(sql: String) = new QueryExecutorDef[R](tree, param){
      override lazy val selectStatement = sql
      override protected def createQueryInvoker[R](tree: Node, param: Any) = super.createQueryInvoker[R](tree, param).hackSql(sql)
    }

    protected def createQueryInvoker[R](tree: Node, param: Any) = driver.createQueryInvoker[R](tree, param)

    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql

    def run(implicit session: Backend#Session): R = tree match {
      case rsm @ ResultSetMapping(_, _, CompiledMapping(_, elemType)) :@ CollectionType(cons, el) =>
        val b = cons.createBuilder(el.classTag).asInstanceOf[Builder[Any, R]]
        createQueryInvoker[Any](rsm, param).foreach({ x => b += x }, 0)(session)
        b.result()
      case First(rsm: ResultSetMapping) =>
        createQueryInvoker[R](rsm, param).first
    }
  }
}

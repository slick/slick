package scala.slick.driver

import scala.slick.ast.Node
import scala.slick.compiler.CodeGen
import scala.slick.jdbc.{PositionedParameters, PositionedResult, StatementInvoker}
import scala.slick.lifted.Shape
import scala.slick.util.{SQLBuilder, ValueLinearizer, CollectionLinearizer, RecordLinearizer}
import java.sql.PreparedStatement

trait JdbcExecutorComponent { driver: JdbcDriver =>

  class QueryExecutor[R](tree: Node, linearizer: ValueLinearizer[_]) {

    def selectStatement = sres.sql

    protected val (_, sres: SQLBuilder.Result) = CodeGen.findResult(tree)

    def run(implicit session: Backend#Session): R = {
      val i = new StatementInvoker[Unit, Any] {
        protected def getStatement = sres.sql
        protected def setParam(param: Unit, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), null)
        protected def extractValue(rs: PositionedResult) = linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[Any]].getResult(driver, rs)
      }
      val res = linearizer match {
        case _: RecordLinearizer[_] => i.first()
        case c =>
          val builder = c.asInstanceOf[CollectionLinearizer[Any, Any]].canBuildFrom()
          i.foreach((), builder += _)
          builder.result()
      }
      res.asInstanceOf[R]
    }
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

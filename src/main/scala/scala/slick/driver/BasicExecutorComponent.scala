package scala.slick.driver

import java.sql.PreparedStatement
import scala.slick.session.{Session, PositionedParameters, PositionedResult}
import scala.slick.lifted.Shape
import scala.slick.ast.Node
import scala.slick.jdbc.StatementInvoker
import scala.collection.mutable.Builder
import slick.util.{ValueLinearizer, CollectionLinearizer, RecordLinearizer}

trait BasicExecutorComponent { driver: BasicDriver =>

  class QueryExecutor[R](input: QueryBuilderInput) {

    def _selectStatement = sres.sql //TODO This should eventually replace StatementInvoker.selectStatement

    protected lazy val sres = createQueryBuilder(input).buildSelect()

    def run(implicit session: Session): R = {
      val i = new StatementInvoker[Unit, Any] {
        protected def getStatement = sres.sql
        protected def setParam(param: Unit, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), null)
        protected def extractValue(rs: PositionedResult) = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[Any]].getResult(driver, rs)
      }
      val res = sres.linearizer match {
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
    @inline def run[U](implicit shape: Shape[M, U, _], session: Session): U =
      Implicit.recordToQueryExecutor(value).run
  }

  // Work-around for SI-3346
  final class ToQueryExecutor[M](val value: M) {
    @inline def toQueryExecutor[U](implicit shape: Shape[M, U, _]): QueryExecutor[U] =
      Implicit.recordToQueryExecutor(value)
  }
}

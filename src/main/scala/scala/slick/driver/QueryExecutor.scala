package scala.slick.driver

import java.sql.PreparedStatement
import scala.slick.session.{Session, PositionedParameters, PositionedResult}
import scala.slick.ql.Shape
import scala.slick.ast.Node
import scala.slick.StatementInvoker
import scala.collection.mutable.Builder
import slick.util.{ValueLinearizer, CollectionLinearizer, RecordLinearizer}

class QueryExecutor[R](query: Node, profile: BasicProfile, linearizer: ValueLinearizer[R]) {

  protected lazy val sres = profile.createQueryBuilder(query, linearizer).buildSelect()

  def run(implicit session: Session): R = {
    val i = new StatementInvoker[Unit, Any] {
      protected def getStatement = sres.sql
      protected def setParam(param: Unit, st: PreparedStatement): Unit = sres.setter(new PositionedParameters(st), null)
      protected def extractValue(rs: PositionedResult) = sres.linearizer.narrowedLinearizer.asInstanceOf[RecordLinearizer[Any]].getResult(profile, rs)
    }
    val res = sres.linearizer match {
      case _: RecordLinearizer[_] => i.first()
      case c: CollectionLinearizer[_, _] =>
        val builder = c.canBuildFrom().asInstanceOf[Builder[Any, Any]]
        i.foreach((), builder += _)
        builder.result()
    }
    res.asInstanceOf[R]
  }
}

class UnshapedQueryExecutor[M](value: M, query: Node, profile: BasicProfile) {
  def run[U](implicit shape: Shape[M, U, _], session: Session): U =
    new QueryExecutor[U](query, profile, shape.linearizer(value)).run
}
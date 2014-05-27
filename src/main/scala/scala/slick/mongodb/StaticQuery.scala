package scala.slick.mongodb

import java.sql.PreparedStatement
import scala.slick.jdbc.{PositionedResult, StatementInvoker, Invoker, SetParameter}
import com.mongodb.DBObject

/**
 * User: Dmytro Vynokurov
 * Date: 27.05.14
 * Time: 1:09
 */
class StaticQuery [-P,+R](query: String, pconv: SetParameter[P], rconv: GetResult[R]) extends (P => Invoker[R]) {
  override def apply(param: P): Invoker[R] = new StaticQueryInvoker[P, R](query, pconv, param, rconv)


  //TODO: do we need foreach manually implemented here? - jdbc's static query doesn't have it
  def foreach(func: DBObject => Unit)(implicit session: MongoBackend#Session) = {
    session.execute().foreach(func)
  }

  class StaticQueryInvoker[-P, +R](val getStatement: String, pconv: SetParameter[P], param: P, rconv: GetResult[R]) extends StatementInvoker[R] {
    override protected def setParam(st: PreparedStatement): Unit = ???

    override protected def extractValue(pr: PositionedResult): R = ???
  }
}
object StaticQuery{
  def apply[R](implicit conv: GetResult[R]) = queryNA("")

  def queryNA[R](query: String)(implicit rconv: GetResult[R]) =
    new StaticQuery[Unit, R](query, SetParameter.SetUnit, rconv)
}

//TODO: review
trait GetResult[+T] extends (DBObject => T) { self =>
  override def andThen[A](g: T => A): GetResult[A] =
    new GetResult[A] { def apply(dbObject: DBObject): A = g(self.apply(dbObject)) }
}
object GetResult{
  def apply[T](f: DBObject => T) = new GetResult[T] {
    override def apply(v1: DBObject): T = f(v1)
  }
}
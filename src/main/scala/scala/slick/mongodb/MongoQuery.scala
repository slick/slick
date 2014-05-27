package scala.slick.mongodb

import java.sql.PreparedStatement
import scala.slick.jdbc.{PositionedResult, StatementInvoker, Invoker, SetParameter}
import com.mongodb.DBObject

// TODO: check if we really need pconv, rconv and invoker here
class MongoQuery [-P,+R](collection:String,query: Option[String], pconv: SetParameter[P], rconv: GetResult[R]) extends (P => Invoker[R]) {
  override def apply(param: P): Invoker[R] = new StaticQueryInvoker[P, R](query.get, pconv, param, rconv)

  //TODO: do we need foreach manually implemented here? - jdbc's static query doesn't have it
  def foreach(func: DBObject => Unit)(implicit session: MongoBackend#Session) = {
    session.find(collection).foreach(func)
  }

  // TODO: implement query invoker
  class StaticQueryInvoker[-P, +R](val getStatement: String, pconv: SetParameter[P], param: P, rconv: GetResult[R]) extends StatementInvoker[R] {
    override protected def setParam(st: PreparedStatement): Unit = ???

    override protected def extractValue(pr: PositionedResult): R = ???
  }
}
// TODO: think how collection name may be received implicitly from result type
// Example: instead of
// Q.queryNA[Employee]("employee","{name:John}") foreach{}    // filters will probably be implemented in a different way
// I wan't to see
// Q.queryNA[Emplyee]("{name:John}") foreach{}
// however, this conflicts with
// Q.queryNA[Employee]("employee") foreach{}
// which is also useful when you don't want to apply any filters
object MongoQuery{
  def apply[R](collection:String)(implicit conv: GetResult[R]) = queryNA(collection)

  def queryNA[R](collection: String, query: String)(implicit rconv: GetResult[R]) =
    new MongoQuery[Unit, R](collection, Some(query), SetParameter.SetUnit, rconv)

  def queryNA[R](collection: String)(implicit rconv: GetResult[R]) =
    new MongoQuery[Unit, R](collection, None, SetParameter.SetUnit, rconv)
}

// TODO: review - probably this must be extended with some implicit conversions(like JDBC's) and moved to separate file
// or we should implement some universal functionality that given the type generates mappings between DBObject and Scala object
// the latter is preferable or even the only right way
trait GetResult[+T] extends (DBObject => T) { self =>
  override def andThen[A](g: T => A): GetResult[A] =
    new GetResult[A] { def apply(dbObject: DBObject): A = g(self.apply(dbObject)) }
}
object GetResult{
  def apply[T](f: DBObject => T) = new GetResult[T] {
    override def apply(v1: DBObject): T = f(v1)
  }
}
package scala.slick.mongodb

import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.Implicits._
import scala.util.parsing.json.JSON

// TODO: check if we really need pconv, rconv and invoker here
class MongoQuery [-P,+R](collection:String,query: Option[String], rconv: GetResult[R]) extends Traversable[R] {

  //TODO: do we need foreach manually implemented here? - jdbc's static query doesn't have it
  def foreach[U](f: MongoDBObject => U)(implicit session: MongoBackend#Session) = {
    // cannot pass f to foreach because of implicit conversion from DBObject to MongoDBObject:
    session.find(collection).foreach(dbObject => f(dbObject))
  }

  override def foreach[U](f: (R) => U): Unit = {
    val g: MongoDBObject => R = mongoDBObject => JSON.parseFull(mongoDBObject)
    foreach(g.andThen(f))
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
    new MongoQuery[Unit, R](collection, Some(query), rconv)

  def queryNA[R](collection: String)(implicit rconv: GetResult[R]) =
    new MongoQuery[Unit, R](collection, None, rconv)

}

// TODO: review - probably this must be extended with some implicit conversions(like JDBC's) and moved to separate file
// or we should implement some universal functionality that given the type generates mappings between MongoDBObject and Scala object
// the latter is preferable or even the only right way
trait GetResult[+T] extends (MongoDBObject => T) { self =>
  override def andThen[A](g: T => A): GetResult[A] =
    new GetResult[A] { def apply(MongoDBObject: MongoDBObject): A = g(self.apply(MongoDBObject)) }
}
object GetResult{
  def apply[T](f: MongoDBObject => T) = new GetResult[T] {
    override def apply(v1: MongoDBObject): T = f(v1)
  }
}
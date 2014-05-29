package scala.slick.mongodb

import com.mongodb.casbah.commons.MongoDBObject

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
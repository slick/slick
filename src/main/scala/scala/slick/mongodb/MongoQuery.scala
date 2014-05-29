package scala.slick.mongodb

import com.mongodb.casbah.Implicits._
import com.mongodb.casbah.MongoCursor


// TODO: finish documentation here
/**
 *
 * Data gets fetched when iterator is used for the first time.
 *
 * @param collection Name of the collection in the MongoDB to fetch data from
 * @param query Represents the query that is used to fetch data
 * @param rconv Mapper from MongoDbObject to Scala object
 * @param session Implicit parameter used for connecting to the database
 * @tparam R Result type of the query
 */
// TODO: see if we can refactor to simplify usage of rconv here
// probably we can use Salat to perform conversions automatically
class MongoQuery [+R](collection:String,query: Option[String], rconv: GetResult[R])(implicit session: MongoBackend#Session) extends Iterable[R] {
  lazy val mongoCursor = session.find(collection)

  override def iterator: Iterator[R] = {
    mongoCursorAsIterator(mongoCursor)
  }

  def mongoCursorAsIterator(mongoCursor: MongoCursor):Iterator[R] = new Iterator[R]{
    override def hasNext: Boolean = mongoCursor.hasNext

    override def next(): R = rconv(mongoCursor.next())
  }
}

// TODO: think how collection name may be received implicitly from result type
// Example: instead of
// Q.queryNA[Employee]("employee","{name:John}") foreach{}    // filters will probably be implemented in a different way
// I want to see
// Q.queryNA[Employee]("{name:John}") foreach{}
// however, this conflicts with
// Q.queryNA[Employee]("employee") foreach{}
// which is also useful when you don't want to apply any filters
object MongoQuery{
  def apply[R](collection:String)(implicit rconv: GetResult[R], session: MongoBackend#Session) = queryNA(collection)(rconv,session)

  def queryNA[R](collection: String, query: String)(implicit rconv: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[R](collection, Some(query), rconv)(session)

  def queryNA[R](collection: String)(implicit rconv: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[R](collection, None, rconv)(session)

}

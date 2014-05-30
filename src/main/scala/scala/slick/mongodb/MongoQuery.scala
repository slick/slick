package scala.slick.mongodb

import com.mongodb.casbah.Implicits._
import com.mongodb.casbah.{MongoCollectionBase, MongoCursor}
import com.mongodb.{DBObject, DBCollection, DBCursor}


// TODO: finish documentation here
/**
 *
 * Data is fetched lazily
 *
 * @param collectionName Name of the collection in the MongoDB to fetch data from
 * @param query Represents the query that is used to fetch data
 * @param rconv Mapper from MongoDbObject to Scala object
 * @param session Implicit parameter used for connecting to the database
 * @tparam R Result type of the query
 */
// TODO: see if we can refactor to simplify usage of rconv here
// probably we can use Salat to perform conversions automatically
class MongoQuery[R](collectionName:String,query: Option[String], rconv: GetResult[R])(implicit session: MongoBackend#Session) extends MongoCollectionBase with Iterable[R] {
  lazy val mongoCollection = session.collectionByName(collectionName)
  lazy val mongoCursor = mongoCollection.find()

  override def iterator: Iterator[R] = {
    mongoCursorAsIterator(mongoCursor)
  }

  def mongoCursorAsIterator(mongoCursor: MongoCursor):Iterator[R] = new Iterator[R]{
    override def hasNext: Boolean = mongoCursor.hasNext

    override def next(): R = rconv(mongoCursor.next())
  }

  override type T = DBObject

  override def underlying: DBCollection = mongoCollection.underlying

  override def _newInstance(collection: DBCollection): MongoCollectionBase = new MongoQuery[R](collectionName,query,rconv)(session)

  override def _newCursor(cursor: DBCursor): CursorType = iterator

  override type CursorType = Iterator[R]
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
  def apply[R](collection:String)(implicit rconv: GetResult[R], session: MongoBackend#Session) = query(collection)(rconv,session)

  def query[R](collection: String, query: String)(implicit rconv: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[R](collection, Some(query), rconv)(session)

  def query[R](collection: String)(implicit rconv: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[R](collection, None, rconv)(session)

}

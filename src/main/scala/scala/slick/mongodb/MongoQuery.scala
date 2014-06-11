package scala.slick.mongodb

import scala.language.implicitConversions
import com.mongodb.casbah.Implicits._
import com.mongodb.casbah.{MongoCursor, MongoCollectionBase}
import com.mongodb.{BasicDBObject, DBObject, DBCollection, DBCursor}
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.util.JSON
import scala.annotation.unchecked.uncheckedVariance


// TODO: finish documentation here
/**
 *
 * MongoQuery points to single collection in a MongoDB
 * However, every instance of the collection iterator operates with it's own database cursor.
 * This provides multithreading
 *
 * @param collectionName Name of the collection in the MongoDB to fetch data from
 * @param filter Represents the query that is used to fetch data
 * @param converter Mapper from MongoDbObject to Scala object
 * @param session Implicit parameter used for connecting to the database
 * @tparam R Result type of the query
 */
// TODO: see if we can refactor to simplify usage of converter here - probably we can use Salat to perform conversions automatically
class MongoQuery[-P,+R](val collectionName:String,val filter: Option[String])(implicit session: MongoBackend#Session, converter: GetResult[R]) extends Iterable[R] with MongoCollectionBase{
  lazy val mongoCollection = session.collectionByName(collectionName)
  def mongoCursor = filter match {
    case Some(f) => mongoCollection.find(JSON.parse(f).asInstanceOf[DBObject])
    case None => mongoCollection.find()
  }

  override def iterator: Iterator[R] = new MongoIterator[R](mongoCursor)(converter)

  // Required for MongoCollectionBase extending:
  def underlying = mongoCollection.underlying
  override type T = DBObject
  override def _newInstance(collection: DBCollection): MongoCollectionBase = new MongoQuery[P,R](collectionName,filter)(session,converter)
  override def _newCursor(cursor: DBCursor): CursorType = iterator
  override type CursorType = Iterator[R @uncheckedVariance] // TODO: review if covariance may cause errors
}

// TODO: think how collection name may be received implicitly from result type - probably some macro required
// Example: instead of
// Q.query[Employee]("employee","{name:John}") foreach{...}
// I want to see
// Q.query[Employee]("{name:John}") foreach{...}
// however, this conflicts with
// Q.query[Employee]("employee") foreach{...}
// which is also useful when you don't want to apply any filters
object MongoQuery{
  implicit def mongoDBObjectAsR[R](mongoDBObject: MongoDBObject)(implicit converter: GetResult[R]):R = converter(mongoDBObject)
  implicit def DBObjectAsR[R](dbObject: DBObject)(implicit converter: GetResult[R]):R = converter(new MongoDBObject(dbObject))

  def apply[R](collection:String)(implicit converter: GetResult[R], session: MongoBackend#Session) = query(collection)(converter,session)

  def apply[P,R](collection:String,filter:String)(implicit converter: GetResult[R], session: MongoBackend#Session) = query(collection,filter)(converter,session)

  private def query[R](collection: String)(implicit converter: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[Unit,R](collection, None)(session, converter)

  def query[P,R](collection: String, filter: String)(implicit converter: GetResult[R], session: MongoBackend#Session) =
    new MongoQuery[P,R](collection, Some(filter))(session, converter)
}

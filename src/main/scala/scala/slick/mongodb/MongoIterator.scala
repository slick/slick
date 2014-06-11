package scala.slick.mongodb

import com.mongodb.casbah.MongoCursor
import scala.language.implicitConversions
import com.mongodb.casbah.Implicits._
import scala.slick.util.CloseableIterator

class MongoIterator[T](val mongoCursor: MongoCursor)(implicit converter: GetResult[T]) extends CloseableIterator[T]{
  override def hasNext: Boolean = mongoCursor.hasNext

  override def next(): T = converter(mongoCursor.next())

  /**
   * Close the underlying data source. The behaviour of any methods of this
   * object after closing it is undefined.
   */
  override def close(): Unit = mongoCursor.close()
}

// TODO: redesign - we should use query with limit clause instead
// While iterating MongoCursor loads documents is small batches
// Limiting on Scala side makes us fetch up to sizeOfBatch - 1 documents that won't be used
class LimitedMongoIterator[T](override val mongoCursor: MongoCursor, val limit: Int)(implicit converter: GetResult[T]) extends MongoIterator[T](mongoCursor){
  var index = 0

  override def hasNext = super.hasNext && (limit!=0 || index<limit)

  override def next(): T = {
    if(!hasNext) throw new IllegalStateException("Iterator has no more values")
    else super.next()
  }
}
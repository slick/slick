package scala.slick.mongodb

import scala.slick.jdbc.{JdbcBackend, Invoker}
import scala.slick.util.CloseableIterator
import com.mongodb.casbah.MongoCursor
import scala.slick.common.GenericInvoker

class MongoInvoker[T](val mongoCursor: MongoCursor)(implicit converter: GetResult[T]) extends GenericInvoker[T]{self=>
  override type Session = MongoBackend#Session

  /** Execute the statement and return the first row of the result set wrapped
    * in Some, or None if the result set is empty. */
  override def firstOption(implicit session: Session): Option[T] = ???

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly.
    * @param maxRows Maximum number of rows to read from the result (0 for unlimited). */
  override def iteratorTo(maxRows: Int)(implicit session: Session): CloseableIterator[T] = ???

  /** Execute the statement and return a CloseableIterator of the converted
    * results. The iterator must either be fully read or closed explicitly. */
  override def iterator(implicit session: Session): CloseableIterator[T] = ???
}

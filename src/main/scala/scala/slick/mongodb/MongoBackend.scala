package scala.slick.mongodb

import scala.slick.backend.DatabaseComponent
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory
import com.mongodb.casbah.Imports._

/**
 * User: Dmytro Vynokurov
 * Date: 21.05.14
 * Time: 20:54
 */
trait MongoBackend extends DatabaseComponent{
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoBackend].getName+".statement"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef {}
  val backend: MongoBackend = this

  class DatabaseDef extends super.DatabaseDef{

    def createConnection(): MongoCollection = ???

    override def createSession(): Session = ???

    override def withTransaction[T](f: Session => T): T = throw new UnsupportedOperationException("Transactions not supported for MongoDB")

    override def withDynTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions not supported for MongoDB")
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef{

  }

  trait SessionDef extends super.SessionDef{
    /** Close this Session. */
    override def close(): Unit = ???

    /** Call this method within a `withTransaction` call to roll back the current
      * transaction after `withTransaction` returns. */
    override def rollback(): Unit = throw new UnsupportedOperationException("Transactions not supported for MongoDB")

    /** Run the supplied function within a transaction. If the function throws an Exception
      * or the session's `rollback()` method is called, the transaction is rolled back,
      * otherwise it is committed when the function returns. */
    override def withTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions not supported for MongoDB")

    /** Force an actual database session to be opened. Slick sessions are lazy, so you do not
      * get a real database connection until you need it or you call force() on the session. */
    override def force(): Unit = ???
  }

}

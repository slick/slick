package scala.slick.mongodb

import scala.slick.backend.DatabaseComponent
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI

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

  // TODO: add possibility to create DatabaseDef without url
  // In case user wants to create the connection with separate
  // parameters: username, password, etc. we don't really need to concatenate
  // them into URI and then pass it to MongoClientURI for parsing
  class DatabaseDef(val connectionUrl:String) extends super.DatabaseDef{

    override def createSession(): Session = new Session(MongoClient(MongoClientURI(connectionUrl)))

    override def withTransaction[T](f: Session => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    override def withDynTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef{
    //TODO: add other methods and parameters here
    def forURL(url: String):DatabaseDef = new DatabaseDef(url)
  }

  class SessionDef(val mongoClient: MongoClient) extends super.SessionDef{
    //TODO: make it take arguments
    def execute() = {
      val db = mongoClient.getDB("test")
      val collection = db("slick_dev")
      collection
    }


    /** Close this Session. */
    override def close(): Unit = mongoClient.close()

    /** Call this method within a `withTransaction` call to roll back the current
      * transaction after `withTransaction` returns. */
    override def rollback(): Unit = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /** Run the supplied function within a transaction. If the function throws an Exception
      * or the session's `rollback()` method is called, the transaction is rolled back,
      * otherwise it is committed when the function returns. */
    override def withTransaction[T](f: => T): T = throw new UnsupportedOperationException("Transactions are not supported by MongoDB")

    /** Force an actual database session to be opened. Slick sessions are lazy, so you do not
      * get a real database connection until you need it or you call force() on the session. */
    override def force(): Unit = throw new UnsupportedOperationException("Mongo session cannot be forced since MongoClient pools connections automatically")
  }

}

object MongoBackend extends MongoBackend {}

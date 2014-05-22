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

  trait DatabaseDef extends super.DatabaseDef{

    def createSession(): Session = new BaseSession(this)

    def createConnection(): MongoCollection
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef{

  }

}

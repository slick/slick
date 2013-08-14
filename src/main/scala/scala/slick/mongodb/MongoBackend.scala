package scala.slick.mongodb

import com.mongodb.casbah.Imports._
import scala.slick.util.SlickLogger
import org.slf4j.LoggerFactory
import scala.slick.backend.DatabaseComponent
import com.mongodb.casbah.MongoClientURI
import scala.slick.lifted.{Constraint, Index}
import scala.slick.SlickException
import scala.slick.mongodb.MongoProfile.options.MongoCollectionOption
import scala.slick.compiler.InsertCompiler
import scala.slick.ast.Insert

trait MongoBackend extends DatabaseComponent {
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoBackend].getName+".statement"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef



  val Database = new DatabaseFactoryDef {}

  val backend: MongoBackend = this

  trait DatabaseDef extends super.DatabaseDef {
    /**
     * Session accessible DatabaseCapabilities - we can probably use this
     * to feed things like JavaScript engine type, etc.
     * Access does not need to be synchronized
     * because, in the worst case, capabilities will be determined multiple
     * times by different concurrent sessions but the result should always be
     * the same.
     */
    @volatile
    protected[MongoBackend] var capabilities: DatabaseCapabilities = null
    /**  For consistency, although in Mongo they like to call 'em collections
      *  ( I admit to usually using the term 'tables' anyway... )
      *  NOTE: Mongo will implicitly create collections that don't exist, if data is inserted...
      *  so there is a strictness issue we have to find a way to consider
      **/
    def getTable(name: String): MongoCollection = createConnection()(name)

    def createTable(name: String, options: Seq[MongoCollectionOption], indexes: IndexedSeq[Index],
                    /* todo - can we support constraints? */ constraints: IndexedSeq[Constraint]): MongoCollection = {
      val conn = createConnection()
      if (conn.collectionExists(name)) throw new SlickException("MongoDB Collection $name already exists.")
      val opts = {
        import MongoProfile.options._
        val b = MongoDBObject.newBuilder
        options match {
          case CollectionCapped =>
            b += "capped" -> true
          case CollectionAutoIndexID(value) =>
            b += "autoIndexID" -> value
          case CollectionSizeBytes(value) =>
            b += "size" -> value
          case CollectionMaxDocuments(value) =>
            b += "max" -> value
        }
        b.result()
      }
      conn.createCollection(name, opts).asScala
      // TODO - CREATE INDEXES AND CONSTRAINTS
    }

    def dropTable(name: String): Unit = getTable(name).dropCollection()

    def getTables: Seq[MongoCollection] = {
      val conn = createConnection()
      for (c <- conn.collectionNames) yield conn(c)
    }

    def createSession(): Session = new Session(this)

    /**
     * The scope of an actually "usable" MongoDB connection is around a database/collection
     * so we don't actually work with a raw connection
     *
     * NOTE: MongoDB driver provides a connection pool automatically so reusing this call is safe.
     * MongoDB object = "MongoDB Database"
     * @return
     */
    def createConnection(): MongoDB
  }

  trait DatabaseFactoryDef extends super.DatabaseFactoryDef {
    /**
     * Creates a connection for a MongoDB URI
     *
     * @see <INSERT LINK TO URI FORMAT DOCS>
     */
    def forURI(uri: String): DatabaseDef = new DatabaseDef {

      val mongoURI: MongoClientURI = MongoClientURI(uri)

      def createConnection(): MongoDB = MongoClient(mongoURI).getDB(mongoURI.database.get /** this will blow up if no DB is defined, should fix */)
    }

    def forConnection(host: String = "localhost", port: Int = 27017, database: String = "test",
                      username: Option[String] = None, password: Option[String] = None)  = new DatabaseDef {
      def createConnection(): MongoDB = {
        val db = MongoClient(host, port).getDB(database)
        if (username.isDefined && password.isDefined)
          if (db.authenticate(username.get, password.get)) db else throw new IllegalArgumentException("Failed to authenticate connection.")
        else if (username.isDefined || password.isDefined)
          throw new IllegalArgumentException("Both username and password must be defined, or neither!")
        else
          db
      }
    }
  }

  trait SessionDef extends super.SessionDef { self =>

    def database: Database
    def conn: MongoDB

    // todo - just aping these from the JDBC backend for now, investigate what we need to do.

    def close(): Unit

    /**
     * MongoDB Does not support transactions, and will throw an exception if invoked.
     */
    def rollback(): Unit = throw new UnsupportedOperationException("MongoDB Does Not Support Transactional Operations!")

    def withTransaction[T](f: => T): T = throw new UnsupportedOperationException("MongoDB Does Not Support Transactional Operations!")

    def force() { conn }
  }

  class Session(val database: Database) extends SessionDef {
    protected var open = false

    lazy val conn = { open = true; database.createConnection() }

    // TODO - server detected capabilities
    // def capabilities

    def close() {
      /**
       * NOOP
       * 1) The connection isn't accessible from the DB level
       * 2) Mongo uses an autmoatic connection pool that can't be properly closed in this way.
       * Just mark it closed and move on, for now.
       * TODO - Better way to handle this?
       */
      open = false
    }

  }

  class DatabaseCapabilities(session: Session) {
    // todo - populate me with some cool stuff
  }



}

object MongoBackend extends MongoBackend {}

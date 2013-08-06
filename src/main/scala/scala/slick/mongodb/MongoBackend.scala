package scala.slick.mongodb

import com.mongodb.casbah.Imports._

trait MongoBackend extends DatabaseComponent {
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoBackend].getName+".statement"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef {}

  val backend: MongoBackend = this

  trait MongoDatabaseDef extends super.DatabaseDef {
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
      *  ( I admit to usually using the term 'tables' anyway... ) */
    protected val tables =

    def createSession(): Session = new MongoSession(this)

    /**
     * The scope of an actually "usable" MongoDB connection is around a database/collection
     * so we don't actually work with a raw connection
     *
     * MongoDB object = "MongoDB Database"
     * @return
     */
    def createConnection(): MongoDB
  }

  trait MongoDatabaseFactoryDef extends super.DatabaseFactoryDef {
    /**
     * Creates a connection for a MongoDB URI
     *
     * @see <INSERT LINK TO URI FORMAT DOCS>
     */
    def forURI(uri: String): DatabaseDef = new MongoDatabaseDef {

      val mongoURI: MongoClientURI = MongoClientURI(uri)

      def createConnection(): MongoDB = MongoClient(mongoURI).getDB(mongoURI.database.get /** this will blow up if no DB is defined, should fix */)
    }

    def forConnection(host: String = "localhost", port: Int = 27017, database: String = "test",
                      username: Option[String] = None, password: Option[String] = None)  = new MongoDatabaseDef {
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

  trait MongoSessionDef extends super.SessionDef { self =>

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

  class MongoSession(val database: Database) extends MongoSessionDef {
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

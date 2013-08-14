package scala.slick.mongodb

import com.mongodb.casbah.Imports._
import scala.slick.util.{Logging, SlickLogger}
import org.slf4j.LoggerFactory
import scala.slick.backend.DatabaseComponent
import com.mongodb.casbah.MongoClientURI
import scala.slick.lifted.{Constraint, Index}
import scala.slick.{mongodb, SlickException}
import scala.slick.mongodb.MongoProfile.options.MongoCollectionOption
import scala.slick.compiler.InsertCompiler
import scala.slick.ast.{ColumnOption, FieldSymbol, Insert}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

trait MongoBackend extends DatabaseComponent {
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[MongoBackend].getName+".statement"))

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef


  val Database = new DatabaseFactoryDef {}

  type Row = DBObject
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

    protected val tables = new mutable.HashMap[String, MongoTable]
    /**  For consistency, although in Mongo they like to call 'em collections
      *  ( I admit to usually using the term 'tables' anyway... )
      *  NOTE: Mongo will implicitly create collections that don't exist, if data is inserted...
      *  so there is a strictness issue we have to find a way to consider
      **/
    def getTable(name: String): MongoTable = {
      tables.get(name).getOrElse(throw new SlickException(s"Table $name does not exist!"))
    }

    def tableExists(name: String): Boolean = createConnection().collectionExists(name)

    def createTable(name: String, columns: Seq[MongoBackend.Column], options: Seq[MongoCollectionOption], indexes: IndexedSeq[Index],
                    /* todo - can we support constraints? */ constraints: IndexedSeq[Constraint]): MongoTable = {
      val conn = createConnection()
      // todo - this is messy, and if something changes underneath us could break... caching, basically, blindly
      if (tables.contains(name) || conn.collectionExists(name)) throw new SlickException("MongoDB Collection $name already exists.")
      val opts = {
        import MongoProfile.options._
        val b = MongoDBObject.newBuilder
        for (option <- options)  option match {
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
      val coll = conn.createCollection(name, opts).asScala
      val t = new MongoTable(coll, options, columns, indexes, constraints)
      // TODO - CREATE INDEXES AND CONSTRAINTS
      tables += ((name, t))
      t
    }

    def dropTable(name: String): Unit = {
      tables.remove(name) match {
        case Some(table) =>
          table.collection.dropCollection()
        case None =>
          throw new SlickException(s"Table $name does not exist!")
      }
    }

    def getTables: IndexedSeq[MongoTable] = {
      /*val conn = createConnection()
      (for (c <- conn.collectionNames) yield conn(c)).toSet*/
      tables.values.toVector
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

  class SessionDef(val database: Database) extends super.SessionDef { self =>
    protected var open = false

    lazy val conn: MongoDB = { open = true; database.createConnection() }

    /**
     * MongoDB Does not support transactions, and will throw an exception if invoked.
     */
    def rollback(): Unit = throw new UnsupportedOperationException("MongoDB Does Not Support Transactional Operations!")

    def withTransaction[T](f: => T): T = throw new UnsupportedOperationException("MongoDB Does Not Support Transactional Operations!")

    def force() { conn }


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

  class MongoTable(private[mongodb] val collection: MongoCollection, val options: Seq[MongoCollectionOption],
                   val columns: Seq[MongoBackend.Column], val indexes: Seq[Index], val constraints: Seq[Constraint]) extends Logging {

    def name = collection.name

    /**
     * A MongoCollection is an Iterable[Row] - iterating it does the equiv of find()
     */
    def rows: Iterable[Row] = collection

    def append(row: Row): Unit = {
      collection.insert(row)
      logger.debug("Inserted (" + row.mkString(", ")+") into " + this)
    }


    def createInsertableDocument: DBObject = {
      val b = MongoDBObject.newBuilder
      for (col <- columns; value <- col.createDefault) b += col.name -> value
      val doc = b.result()
      logger.debug("Created Default Insertable Document '" + doc + "'")
      doc
    }

    //def insert(value: Any, converter: MongoProfile#ResultConverter) = {

    lazy val columnIndexes = columns.map(_.sym).zipWithIndex.toMap


    override def toString = name + "(" + columns.map(_.sym.name).mkString(", ") + ")"
  }


}

object MongoBackend extends MongoBackend {
  class Column(val sym: FieldSymbol, val tpe: MongoType[Any]) {
    def name = sym.name
    private[this] val default = sym.options.collectFirst { case ColumnOption.Default(v) => v }
    private[this] val autoInc = sym.options.collectFirst { case ColumnOption.AutoInc =>
      throw new SlickException("MongoDB does not support Auto Increment columns. For similar functionality, please use an ObjectID type.")
    }

    // todo - are we looking for UNIQUENESS or primary key?!
    val isUnique = sym.options.collectFirst { case ColumnOption.PrimaryKey => true }.getOrElse(false)

    def createDefault: Option[Any] = default // TODO - Find a more elegant way to handle this than a deliberate blowup-if-not


  }
}

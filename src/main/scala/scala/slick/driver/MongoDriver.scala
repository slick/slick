package scala.slick.driver

import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.TypeUtil._
import scala.slick.util.{SlickLogger, SQLBuilder}
import scala.slick.profile._
import scala.slick.backend.DatabaseComponent
import scala.slick.jdbc.JdbcBackend
import org.slf4j.LoggerFactory

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI
import com.mongodb.CommandResult

/**
 * Slick driver for MongoDB
 *
 * Based on Casbah, Fully synchronous. A rough sketch of the ultimate plan for a full fledged
 * MongoDB mapping
 *
 * @author bwmcadams
 */
class MongoDriver extends RelationalDriver with MongoProfile { driver =>

  // TODO - Detect Mongo JS Engine and access to aggregation at connection time
  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalProfile.capabilities.foreignKeyActions /** we might be able to gerryMander this around DBRefs, later */
    - RelationalProfile.capabilities.functionDatabase /** I think we need to be able to do this in an exact SQL Way...? */
    - RelationalProfile.capabilities.functionUser
    - RelationalProfile.capabilities.joinFull
    - RelationalProfile.capabilities.joinRight
    - RelationalProfile.capabilities.likeEscape
    - RelationalProfile.capabilities.pagingDrop
    - RelationalProfile.capabilities.pagingNested
    - RelationalProfile.capabilities.pagingPreciseTake
    - RelationalProfile.capabilities.typeBigDecimal /** MongoDB has no safe type mapping for BigDecimal */
    - RelationalProfile.capabilities.zip /** TODO - talk to Stefan about what zip capabilities means/need */
  )

}

object MongoProfile {
  object capabilities {
    /** Supports aggregation framework */
    val aggregation = Capability("mongodb.aggregation")
    /** Supports the V8 JavaScript engine */
    val v8JSEngine = Capability("mongodb.jsengine.v8")
    /** Supports the SpiderMonkey JavaScript engine  (v8 is better and allows more features ) */
    val spiderMonkeyJSEngine = Capability("mongodb.jsengine.spidermonkey")

    /** Supports all MongoDB features which do not have separate capability values */
    val other = Capability("mongodb.other")

    /** All MongoDB capabilities */
    val all = Set(aggregation, v8JSEngine, spiderMonkeyJSEngine, other)
  }
}


trait MongoProfile extends RelationalProfile with MongoInvokerComponent with MongoExecutorComponent with MongoTableComponent { driver: MongoDriver =>
  type Backend = MongoBackend
}


trait MongoBackend extends DatabaseComponent {
  protected[this] lazy val statementLogger = new SlickLogger(LoggerFactory.getLogger(classOf[JdbcBackend].getName+".statement"))

  type Database = MongoDatabaseDef
  type Session = MongoSessionDef
  type DatabaseFactory = MongoDatabaseFactoryDef

  val Database = new MongoDatabaseFactoryDef {}

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

trait MongoExecutorComponent extends BasicExecutorComponent { driver: MongoDriver =>

  type QueryExecutor[T] <: QueryExecutorDef[T]

  def createQueryExecutor[R](tree: Node, param: Any): QueryExecutor[R] = new MongoQueryExecutorDef[R](tree, param)

  class MongoQueryExecutorDef[R](tree: Node, param: Any) extends super.QueryExecutorDef[R] {

    // TODO - Make me something Mongo-ey
    lazy val selectStatement =
      tree.findNode(_.isInstanceOf[CompiledStatement]).get
        .asInstanceOf[CompiledStatement].extra.asInstanceOf[SQLBuilder.Result].sql // TODO - Obviously we don't want a bleeding SQLBuilder

    def run(implicit session: Backend#Session): R = { tree match {
      // see JdbcExecutorComponent L:21
      case rsm: ResultSetMapping =>
        createMongoQueryInvoker[Any, Any](rsm).to(param)(session, rsm.nodeType.asCollectionType.cons.canBuildFrom)
      case First(rsm: ResultSetMapping) =>
        createMongoQueryInvoker[Any, Any](rsm).first(param)
    }}.asInstanceOf[R]
  }
}

trait MongoInvokerComponent extends BasicInvokerComponent { driver: MongoDriver =>
  type InsertInvoker[T] = MongoInsertInvoker[T]

  def createInsertInvoker[U](tree: Node) = createMongoInsertInvoker(tree)

  // Create the different invokers –– these methods should be overriden by drivers as needed
  def createMongoInsertInvoker[I](tree: Node) = new MongoInsertInvoker[I](tree)
  def createMongoCommandInvoker[C](tree: Node) = new MongoCommandInvoker[C](tree)
  def createMongoUpdateInvoker[U](tree: Node) = new MongoUpdateInvoker[U](tree)
  def createMongoDeleteInvoker[D](tree: Node) = new MongoDeleteInvoker[D](tree)
  def createMongoQueryInvoker[P, R](tree: Node): MongoQueryInvoker[P, R] = new MongoQueryInvoker[P, R](tree)

  // Parameters for invokers -- can be overriden blah blah blah
  // TODO - see JdbcInvokerComponent L: 26 - we may want to model similarly

  /** A parameterized query invoker */
  class MongoQueryInvoker[P, R](protected val tree: Node) extends MongoInvoker[P, R] {
    protected[this] val ResultSetMapping(_ , _ , _ /* TODO - FILL ME IN
      CompiledStatement(_, sres: SQLBuilder.Result),
      CompiledMapping() */) = tree

    def queryObject: DBObject = ???
    def invoker: this.type = this
  }

  /** Invoker for executing commands, which return Unit */
  class MongoCommandInvoker[C](tree: Node) extends MongoQueryInvoker[Unit, C](tree) {
    def commandObject: DBObject = ???
  }

  /** Pseudo-invoker for running Deletions */
  class MongoDeleteInvoker(protected val tree: Node) {
    protected[this] val ResultSetMapping(_, _, _) = tree

    def deleteObject: DBObject = ???
    def delete(implicit session: Backend#Session): WriteResult = ???

    def deleteInvoker: this.type = this
  }

  /** Pseudo-Invoker for insertions
    * TODO - Propertly support UPSERT
    */
  class MongoInsertInvoker[I](tree: Node) {

    protected[this] val ResultSetMapping(_, insertNode: Insert, _ /*CompiledMapping(converter, _) */) = tree
    protected[this] lazy val builder = createInsertBuilder(insertNode)

    protected lazy val insertResult = builder.buildInsert
    lazy val insertObject: DBObject = insertResult.toDBObject

    // TODO - insertStatementFor Shape???

    def insert(value: I)(implicit session: Backend#Session): Unit = ???

    /** Insert multiple rows. Uses the Mongo driver batch feature.
      * Right now, no return - just exceptions thrown if DB tosses an error.
      */
    def insertAll(values: I*)(implicit session: Backend#Session): Unit = ???

    /** Insert a single value */
    def +=(value: I)(implicit session: Backend#Session): Unit = insert(value)

    /** Insert a collection of values */
    def ++=(values: Iterable[I])(implicit session: Backend#Session): Unit = insertAll(values.toSeq: _*)

    def insertInvoker: this.type = this
  }

  /** Pseudo-Invoker for running Updates.
    * TODO - Upsert support ?
    *
    */
  class MongoUpdateInvoker[U](protected val tree: Node) {
    protected[this] val ResultSetMapping(_, _, _) = tree // TODO - fill me out

    def updateObject: DBObject = ???

    def update(value: U)(implicit session: Backend#Session): Unit = ???

    def updateInvoker: this.type = this
  }
}

/** Base trait for Mongo Statement invokers, using parameter type P & Result type R */
trait MongoInvoker[-P, +R] extends Function1[P, MongoUnitInvoker[R]] { self =>

  /**
   * Execute the statement & return a CloseableIterator of the converted results.
   * The iterator must be either fully ready or closed explicitly (In MongoDB's case the iterator *should*
   * get automatically cleaned up)
   */


}

trait MongoUnitInvoker[+R]

trait MongoTableComponent extends RelationalTableComponent { driver: MongoDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions
}
package slick.test.ce

import java.io.PrintWriter
import java.sql.{Connection, SQLException}
import java.util.concurrent.{Semaphore as JSemaphore, TimeUnit}
import java.util.logging.Logger
import javax.sql.DataSource

import scala.concurrent.duration.*

import munit.CatsEffectSuite

import slick.cats.Database
import slick.ControlsConfig
import slick.jdbc.DatabaseConfig
import slick.jdbc.{DataSourceJdbcDataSource, DriverDataSource, H2Profile}
import slick.jdbc.H2Profile.api.*

/** Tests that `keepAliveConnection = true` works correctly with bounded connection pools.
  *
  * The keep-alive connection prevents named in-memory databases (e.g. H2, HSQLDB) from
  * being destroyed when no other connections are open.  It is opened eagerly during
  * `Resource.make` and held for the lifetime of the database.
  *
  * The keep-alive connection is not counted against `maxConnections`.  When the underlying
  * `DataSource` is bounded (e.g. a connection pool), it must be sized to at least
  * `maxConnections + 1` to accommodate the extra connection.
  */
class KeepAliveDeadlockTest extends CatsEffectSuite {

  // Give the test enough time to prove liveness *or* detect the deadlock.
  // The actual timeout for the deadlocking operation is much shorter (see below).
  override def munitIOTimeout: Duration = 30.seconds

  /** A [[DataSource]] wrapper that limits the number of concurrently open connections
    * using a counting semaphore — simulating a bounded connection pool (like HikariCP).
    *
    * `getConnection` blocks when the pool is exhausted, exactly like a real pool would.
    */
  private class BoundedDataSource(underlying: DataSource, maxSize: Int) extends DataSource {
    private val available = new JSemaphore(maxSize)

    override def getConnection: Connection = {
      // Use a timed acquire so the test can detect pool exhaustion
      // instead of hanging forever. A real pool (HikariCP) behaves similarly,
      // throwing after its connectionTimeout.
      if (!available.tryAcquire(5, TimeUnit.SECONDS))
        throw new SQLException(
          "BoundedDataSource: pool exhausted (all " + maxSize + " connections in use)")
      val conn = underlying.getConnection
      new ConnectionWrapper(conn) {
        private var closed = false
        override def close(): Unit = if (!closed) {
          closed = true
          conn.close()
          available.release()                 // return slot to pool
        }
      }
    }

    override def getConnection(username: String, password: String): Connection =
      throw new UnsupportedOperationException

    // -- boilerplate required by DataSource --
    override def getLogWriter: PrintWriter = underlying.getLogWriter
    override def setLogWriter(out: PrintWriter): Unit = underlying.setLogWriter(out)
    override def setLoginTimeout(seconds: Int): Unit = underlying.setLoginTimeout(seconds)
    override def getLoginTimeout: Int = underlying.getLoginTimeout
    override def getParentLogger: Logger = underlying.getParentLogger
    override def isWrapperFor(iface: Class[?]): Boolean = underlying.isWrapperFor(iface)
    override def unwrap[T](iface: Class[T]): T = underlying.unwrap(iface)
  }

  /** Minimal Connection wrapper that delegates everything to an underlying connection.
    * Subclassed by BoundedDataSource to intercept `close()`.
    */
  private abstract class ConnectionWrapper(delegate: Connection) extends Connection {
    // -- resource lifecycle --
    override def close(): Unit = delegate.close()
    override def isClosed: Boolean = delegate.isClosed

    // -- transaction control --
    override def setAutoCommit(autoCommit: Boolean): Unit = delegate.setAutoCommit(autoCommit)
    override def getAutoCommit: Boolean = delegate.getAutoCommit
    override def commit(): Unit = delegate.commit()
    override def rollback(): Unit = delegate.rollback()
    override def rollback(savepoint: java.sql.Savepoint): Unit = delegate.rollback(savepoint)
    override def setSavepoint(): java.sql.Savepoint = delegate.setSavepoint()
    override def setSavepoint(name: String): java.sql.Savepoint = delegate.setSavepoint(name)
    override def releaseSavepoint(savepoint: java.sql.Savepoint): Unit = delegate.releaseSavepoint(savepoint)
    override def setTransactionIsolation(level: Int): Unit = delegate.setTransactionIsolation(level)
    override def getTransactionIsolation: Int = delegate.getTransactionIsolation

    // -- statement creation --
    override def createStatement(): java.sql.Statement = delegate.createStatement()
    override def createStatement(resultSetType: Int, resultSetConcurrency: Int): java.sql.Statement =
      delegate.createStatement(resultSetType, resultSetConcurrency)
    override def createStatement(resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): java.sql.Statement =
      delegate.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability)
    override def prepareStatement(sql: String): java.sql.PreparedStatement = delegate.prepareStatement(sql)
    override def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int): java.sql.PreparedStatement =
      delegate.prepareStatement(sql, resultSetType, resultSetConcurrency)
    override def prepareStatement(sql: String, resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): java.sql.PreparedStatement =
      delegate.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability)
    override def prepareStatement(sql: String, autoGeneratedKeys: Int): java.sql.PreparedStatement =
      delegate.prepareStatement(sql, autoGeneratedKeys)
    override def prepareStatement(sql: String, columnIndexes: Array[Int]): java.sql.PreparedStatement =
      delegate.prepareStatement(sql, columnIndexes)
    override def prepareStatement(sql: String, columnNames: Array[String]): java.sql.PreparedStatement =
      delegate.prepareStatement(sql, columnNames)
    override def prepareCall(sql: String): java.sql.CallableStatement = delegate.prepareCall(sql)
    override def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int): java.sql.CallableStatement =
      delegate.prepareCall(sql, resultSetType, resultSetConcurrency)
    override def prepareCall(sql: String, resultSetType: Int, resultSetConcurrency: Int, resultSetHoldability: Int): java.sql.CallableStatement =
      delegate.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability)

    // -- metadata and properties --
    override def getMetaData: java.sql.DatabaseMetaData = delegate.getMetaData
    override def setCatalog(catalog: String): Unit = delegate.setCatalog(catalog)
    override def getCatalog: String = delegate.getCatalog
    override def setSchema(schema: String): Unit = delegate.setSchema(schema)
    override def getSchema: String = delegate.getSchema
    override def setReadOnly(readOnly: Boolean): Unit = delegate.setReadOnly(readOnly)
    override def isReadOnly: Boolean = delegate.isReadOnly
    override def setHoldability(holdability: Int): Unit = delegate.setHoldability(holdability)
    override def getHoldability: Int = delegate.getHoldability
    override def getWarnings: java.sql.SQLWarning = delegate.getWarnings
    override def clearWarnings(): Unit = delegate.clearWarnings()
    override def getTypeMap: java.util.Map[String, Class[?]] = delegate.getTypeMap
    override def setTypeMap(map: java.util.Map[String, Class[?]]): Unit = delegate.setTypeMap(map)
    override def isValid(timeout: Int): Boolean = delegate.isValid(timeout)
    override def getClientInfo(name: String): String = delegate.getClientInfo(name)
    override def getClientInfo: java.util.Properties = delegate.getClientInfo
    override def setClientInfo(name: String, value: String): Unit = delegate.setClientInfo(name, value)
    override def setClientInfo(properties: java.util.Properties): Unit = delegate.setClientInfo(properties)
    override def nativeSQL(sql: String): String = delegate.nativeSQL(sql)

    // -- large / advanced --
    override def createClob(): java.sql.Clob = delegate.createClob()
    override def createBlob(): java.sql.Blob = delegate.createBlob()
    override def createNClob(): java.sql.NClob = delegate.createNClob()
    override def createSQLXML(): java.sql.SQLXML = delegate.createSQLXML()
    override def createArrayOf(typeName: String, elements: Array[AnyRef]): java.sql.Array =
      delegate.createArrayOf(typeName, elements)
    override def createStruct(typeName: String, attributes: Array[AnyRef]): java.sql.Struct =
      delegate.createStruct(typeName, attributes)
    override def setNetworkTimeout(executor: java.util.concurrent.Executor, milliseconds: Int): Unit =
      delegate.setNetworkTimeout(executor, milliseconds)
    override def getNetworkTimeout: Int = delegate.getNetworkTimeout
    override def abort(executor: java.util.concurrent.Executor): Unit = delegate.abort(executor)

    // -- wrapper --
    override def isWrapperFor(iface: Class[?]): Boolean = delegate.isWrapperFor(iface)
    override def unwrap[T](iface: Class[T]): T = delegate.unwrap(iface)
  }

  test("keepAliveConnection with maxConnections=1 and pool sized to 2 works") {
    val rawDs = new DriverDataSource(
      "jdbc:h2:mem:keepalive_single;DB_CLOSE_DELAY=-1",
      driverClassName = "org.h2.Driver"
    )
    // maxConnections = 1 means 1 usable connection.  The keep-alive connection is extra,
    // so the pool must be sized to maxConnections + 1 = 2.
    val bounded = new BoundedDataSource(rawDs, maxSize = 2)
    val source = new DataSourceJdbcDataSource(bounded, keepAliveConnection = true, maxConnections = Some(1))

    Database.resource(DatabaseConfig.forSource(H2Profile, source).withControls(ControlsConfig(maxConnections = 1))).use { db =>
      db.run(sql"SELECT 1".as[Int].head).map { r =>
        assertEquals(r, 1)
      }
    }
  }

  test("keepAliveConnection with maxConnections=2 allows 2 concurrent queries") {
    val rawDs = new DriverDataSource(
      "jdbc:h2:mem:keepalive_conc;DB_CLOSE_DELAY=-1",
      driverClassName = "org.h2.Driver"
    )
    // maxConnections = 2 means 2 usable connections.  Pool sized to 2 + 1 = 3.
    val bounded = new BoundedDataSource(rawDs, maxSize = 3)
    val source = new DataSourceJdbcDataSource(bounded, keepAliveConnection = true, maxConnections = Some(2))

    Database.resource(DatabaseConfig.forSource(H2Profile, source).withControls(ControlsConfig(maxConnections = 2))).use { db =>
      import cats.syntax.parallel.*
      (
        db.run(sql"SELECT 1".as[Int].head),
        db.run(sql"SELECT 2".as[Int].head)
      ).parTupled.map { case (a, b) =>
        assertEquals(a, 1)
        assertEquals(b, 2)
      }
    }
  }

  test("pool undersized for keepAliveConnection causes pool exhaustion") {
    val rawDs = new DriverDataSource(
      "jdbc:h2:mem:keepalive_undersized;DB_CLOSE_DELAY=-1",
      driverClassName = "org.h2.Driver"
    )
    // Pool sized to exactly maxConnections (1) without room for the keep-alive connection.
    // The keep-alive grabs the only pool slot, leaving none for actual work.
    val bounded = new BoundedDataSource(rawDs, maxSize = 1)
    val source = new DataSourceJdbcDataSource(bounded, keepAliveConnection = true, maxConnections = Some(1))

    Database.resource(DatabaseConfig.forSource(H2Profile, source).withControls(ControlsConfig(maxConnections = 1))).use { db =>
      db.run(sql"SELECT 1".as[Int].head).attempt.map {
        case Left(e: SQLException) if e.getMessage.contains("pool exhausted") =>
          // Expected: the pool is undersized — it should have been maxConnections + 1.
          ()
        case Left(e) =>
          fail(s"Unexpected error: $e")
        case Right(_) =>
          fail("Expected pool exhaustion but query succeeded")
      }
    }
  }
}

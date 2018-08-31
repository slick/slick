package com.typesafe.slick.testkit.util


import com.typesafe.config.Config


import java.io._
import java.net.{URL, URLClassLoader}
import java.sql.{Connection, Driver}
import java.util.Properties
import java.util.concurrent.ExecutionException
import java.util.zip.GZIPInputStream

import scala.collection.mutable
import scala.concurrent.{Await, Future, ExecutionContext}

import slick.basic.{BasicProfile, Capability}
import slick.dbio.{NoStream, DBIOAction, DBIO}
import slick.jdbc.{JdbcProfile, ResultSetAction, JdbcDataSource}
import slick.jdbc.GetResult._
import slick.relational.RelationalProfile
import slick.sql.SqlProfile
import slick.util.AsyncExecutor

object TestDB {
  object capabilities {
    /** Marks a driver which is specially supported by the test kit for plain SQL queries. */
    val plainSql = new Capability("test.plainSql")
    /** Supports JDBC metadata in general */
    val jdbcMeta = new Capability("test.jdbcMeta")
    /** Supports JDBC metadata getClientInfoProperties method */
    val jdbcMetaGetClientInfoProperties = new Capability("test.jdbcMetaGetClientInfoProperties")
    /** Supports JDBC metadata getFunctions method */
    val jdbcMetaGetFunctions = new Capability("test.jdbcMetaGetFunctions")
    /** Supports JDBC metadata getIndexInfo method */
    val jdbcMetaGetIndexInfo = new Capability("test.jdbcMetaGetIndexInfo")
    /** Supports all tested transaction isolation levels */
    val transactionIsolation = new Capability("test.transactionIsolation")
    /** Supports select for update row locking */
    val selectForUpdateRowLocking = new Capability("test.selectForUpdateRowLocking")

    val all = Set(plainSql, jdbcMeta, jdbcMetaGetClientInfoProperties, jdbcMetaGetFunctions, jdbcMetaGetIndexInfo,
      transactionIsolation, selectForUpdateRowLocking)
  }

  /** Copy a file, expanding it if the source name ends with .gz */
  def copy(src: File, dest: File): Unit = {
    dest.createNewFile()
    val out = new FileOutputStream(dest)
    try {
      var in: InputStream = new FileInputStream(src)
      try {
        if(src.getName.endsWith(".gz")) in = new GZIPInputStream(in)
        val buf = new Array[Byte](4096)
        var cont = true
        while(cont) {
          val len = in.read(buf)
          if(len < 0) cont = false
          else out.write(buf, 0, len)
        }
      } finally in.close()
    } finally out.close()
  }

  /** Delete files in the testDB directory */
  def deleteDBFiles(prefix: String): Unit = {
    assert(!prefix.isEmpty, "prefix must not be empty")
    def deleteRec(f: File): Boolean = {
      if(f.isDirectory()) f.listFiles.forall(deleteRec _) && f.delete()
      else f.delete()
    }
    val dir = new File(TestkitConfig.testDir)
    if(!dir.isDirectory) throw new IOException("Directory "+TestkitConfig.testDir+" not found")
    for(f <- dir.listFiles if f.getName startsWith prefix) {
      val p = TestkitConfig.testDir+"/"+f.getName
      if(deleteRec(f)) println("[Deleted database file "+p+"]")
      else throw new IOException("Couldn't delete database file "+p)
    }
  }

  def mapToProps(m: Map[String, String]) = {
    val p = new Properties
    if(m ne null)
      for((k,v) <- m) if(k.ne(null) && v.ne(null)) p.setProperty(k, v)
    p
  }
}

/**
 * Describes a database against which you can run TestKit tests. It includes
 * features such as reading the configuration file, setting up a DB connection,
 * removing DB files left over by a test run, etc.
 */
trait TestDB {
  type Profile <: BasicProfile

  /** The test database name */
  val confName: String

  /** The test configuration */
  lazy val config: Config = TestkitConfig.testConfig(confName)

  /** Check if this test database is enabled */
  def isEnabled = TestkitConfig.testDBs.map(_.contains(confName)).getOrElse(true)

  /** This method is called to clean up before running all tests. */
  def cleanUpBefore(): Unit = {}

  /** This method is called to clean up after running all tests. It
    * defaults to cleanUpBefore(). */
  def cleanUpAfter() = cleanUpBefore()

  /** The profile for the database */
  val profile: Profile

  /** Indicates whether the database persists after closing the last connection */
  def isPersistent = true

  /** This method is called between individual test methods to remove all
    * database artifacts that were created by the test. */
  def dropUserArtifacts(implicit session: profile.Backend#Session): Unit

  /** Create the Database object for this test database configuration */
  def createDB(): profile.Backend#Database

  /** Indicates whether the database's sessions have shared state. When a
    * database is shared but not persistent, Testkit keeps a session open
    * to make it persistent. */
  def isShared = true

  /** The capabilities of the Slick profile, possibly modified for this
    * test configuration. */
  def capabilities: Set[Capability] = profile.capabilities ++ TestDB.capabilities.all

  def confOptionalString(path: String) = if(config.hasPath(path)) Some(config.getString(path)) else None
  def confString(path: String) = confOptionalString(path).getOrElse(null)
  def confStrings(path: String) = TestkitConfig.getStrings(config, path).getOrElse(Nil)

  /** The tests to run for this configuration. */
  def testClasses: Seq[Class[_ <: GenericTest[_ >: Null <: TestDB]]] = TestkitConfig.testClasses
}

trait RelationalTestDB extends TestDB {
  type Profile <: RelationalProfile

  def assertTablesExist(tables: String*): DBIO[Unit]
  def assertNotTablesExist(tables: String*): DBIO[Unit]
}

trait SqlTestDB extends RelationalTestDB { type Profile <: SqlProfile }

abstract class JdbcTestDB(val confName: String) extends SqlTestDB {
  import profile.api.actionBasedSQLInterpolation

  type Profile = JdbcProfile
  lazy val database = profile.backend.Database
  val jdbcDriver: String
  final def getLocalTables(implicit session: profile.Backend#Session) = blockingRunOnSession(ec => localTables(ec))
  final def getLocalSequences(implicit session: profile.Backend#Session) = blockingRunOnSession(ec => localSequences(ec))
  def canGetLocalTables = true
  def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "", null, null)).map { ts =>
      ts.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
    }
  def localSequences(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "", null, null)).map { ts =>
      ts.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
  def dropUserArtifacts(implicit session: profile.Backend#Session) = blockingRunOnSession { implicit ec =>
    for {
      tables <- localTables
      sequences <- localSequences
      _ <- DBIO.seq((tables.map(t => sqlu"""drop table if exists #${profile.quoteIdentifier(t)} cascade""") ++
        sequences.map(t => sqlu"""drop sequence if exists #${profile.quoteIdentifier(t)} cascade""")): _*)
    } yield ()
  }
  def assertTablesExist(tables: String*) =
    DBIO.seq(tables.map(t => sql"""select 1 from #${profile.quoteIdentifier(t)} where 1 < 0""".as[Int]): _*)
  def assertNotTablesExist(tables: String*) =
    DBIO.seq(tables.map(t => sql"""select 1 from #${profile.quoteIdentifier(t)} where 1 < 0""".as[Int].failed): _*)
  def createSingleSessionDatabase(implicit session: profile.Backend#Session, executor: AsyncExecutor = AsyncExecutor.default()): profile.Backend#Database = {
    val wrappedConn = new DelegateConnection(session.conn) {
      override def close(): Unit = ()
    }
    profile.backend.Database.forSource(new JdbcDataSource {
      def createConnection(): Connection = wrappedConn
      def close(): Unit = ()
      val maxConnections: Option[Int] = Some(1)
    }, executor)
  }
  final def blockingRunOnSession[R](f: ExecutionContext => DBIOAction[R, NoStream, Nothing])(implicit session: profile.Backend#Session): R = {
    val ec = new ExecutionContext {
      def execute(runnable: Runnable): Unit = runnable.run()
      def reportFailure(t: Throwable): Unit = throw t
    }
    val db = createSingleSessionDatabase(session, new AsyncExecutor {
      def executionContext: ExecutionContext = ec
      def close(): Unit = ()
    })
    db.run(f(ec)).value.get.get
  }
  protected[this] def await[T](f: Future[T]): T =
    try Await.result(f, TestkitConfig.asyncTimeout)
    catch { case ex: ExecutionException => throw ex.getCause }
}

abstract class InternalJdbcTestDB(confName: String) extends JdbcTestDB(confName) { self =>
  val url: String
  def createDB(): profile.Backend#Database = database.forURL(url, driver = jdbcDriver)
  override def toString = url
}

abstract class ExternalJdbcTestDB(confName: String) extends JdbcTestDB(confName) {
  import profile.api.actionBasedSQLInterpolation

  val jdbcDriver = confString("driver")
  val testDB = confString("testDB")

  val create = confStrings("create")
  val postCreate = confStrings("postCreate")
  val drop = confStrings("drop")

  override def toString = confString("testConn.url")

  override def isEnabled = super.isEnabled && config.getBoolean("enabled")

  override lazy val testClasses: Seq[Class[_ <: GenericTest[_ >: Null <: TestDB]]] =
    TestkitConfig.getStrings(config, "testClasses")
      .map(_.map(n => Class.forName(n).asInstanceOf[Class[_ <: GenericTest[_ >: Null <: TestDB]]]))
      .getOrElse(super.testClasses)

  def databaseFor(path: String) = database.forConfig(path, config, loadCustomDriver().getOrElse(null))

  override def createDB() = databaseFor("testConn")

  override def cleanUpBefore(): Unit = {
    if(!drop.isEmpty || !create.isEmpty) {
      println("[Creating test database "+this+"]")
      await(databaseFor("adminConn").run(
        DBIO.seq((drop ++ create).map(s => sqlu"#$s"): _*).withPinnedSession
      ))
    }
    if(!postCreate.isEmpty) {
      await(createDB().run(
        DBIO.seq(postCreate.map(s => sqlu"#$s"): _*).withPinnedSession
      ))
    }
  }

  override def cleanUpAfter(): Unit = {
    if(!drop.isEmpty) {
      println("[Dropping test database "+this+"]")
      await(databaseFor("adminConn").run(
        DBIO.seq(drop.map(s => sqlu"#$s"): _*).withPinnedSession
      ))
    }
  }

  def loadCustomDriver() = confOptionalString("driverJar").map { jar =>
    ExternalTestDB.getCustomDriver(jar, jdbcDriver)
  }
}

object ExternalTestDB {
  // A cache for custom drivers to avoid excessive reloading and memory leaks
  private[this] val driverCache = new mutable.HashMap[(String, String), Driver]()

  def getCustomDriver(url: String, driverClass: String) = synchronized {
    val sysloader = java.lang.ClassLoader.getSystemClassLoader
    val sysclass = classOf[URLClassLoader]

    // Add the supplied jar onto the system classpath
    // Doing this allows Hikari to initialise the driver, if needed
    try {
        val method = sysclass.getDeclaredMethod("addURL", classOf[URL])
        method.setAccessible(true)
        method.invoke(sysloader, new URL(url))
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw new IOException(s"Error, could not add URL $url to system classloader");
    }
    driverCache.getOrElseUpdate((url, driverClass),
      new URLClassLoader(Array(new URL(url)), getClass.getClassLoader).loadClass(driverClass).newInstance.asInstanceOf[Driver]
    )
  }
}

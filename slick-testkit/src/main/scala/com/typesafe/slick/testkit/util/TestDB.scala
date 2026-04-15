package com.typesafe.slick.testkit.util

import java.io.*
import java.sql.Connection
import java.util.Properties
import java.util.zip.GZIPInputStream

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import slick.jdbc.DatabaseConfig
import slick.basic.{BasicProfile, Capability}
import slick.util.ClassLoaderUtil
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}
import slick.jdbc.{JdbcDataSource, JdbcProfile, ResultSetAction}
import slick.jdbc.GetResult.*
import slick.relational.RelationalProfile
import slick.sql.SqlProfile

import com.typesafe.config.Config


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
    assert(prefix.nonEmpty, "prefix must not be empty")
    def deleteRec(f: File): Boolean = {
      if(f.isDirectory) f.listFiles.forall(deleteRec) && f.delete()
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
  def isEnabled = TestkitConfig.testDBs.forall(_.contains(confName))

  /** This method is called to clean up before running all tests. */
  def cleanUpBefore(): Unit = {}

  /** This method is called to clean up after running all tests. It
    * defaults to cleanUpBefore(). */
  def cleanUpAfter() = cleanUpBefore()

  /** The profile for the database */
  val profile: Profile

  /** A type alias for the raw backend Database with F fixed to IO */
  type IODatabase = profile.backend.BasicDatabaseDef[IO]

  /** Indicates whether the database persists after closing the last connection */
  def isPersistent = true

  /** This method is called between individual test methods to remove all
    * database artifacts that were created by the test. */
  def dropUserArtifacts(implicit session: profile.backend.Session): Unit

  /** Create the Database object for this test database configuration.
    * Uses Resource.allocated to obtain the Database synchronously for test purposes.
    * The finalizer (close) is called explicitly via db.close(). */
  def createDB(): IODatabase

  /** Indicates whether the database's sessions have shared state. When a
    * database is shared but not persistent, Testkit keeps a session open
    * to make it persistent. */
  def isShared = true

  /** The capabilities of the Slick profile, possibly modified for this
    * test configuration. */
  def capabilities: Set[Capability] = profile.capabilities ++ TestDB.capabilities.all

  def confOptionalString(path: String) = if(config.hasPath(path)) Some(config.getString(path)) else None
  def confString(path: String) = confOptionalString(path).orNull
  def confStrings(path: String) = TestkitConfig.getStrings(config, path).getOrElse(Nil)

  /** The tests to run for this configuration. */
  def testClasses: Seq[Class[? <: AsyncTest[? >: Null <: TestDB]]] = TestkitConfig.testClasses
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
  val jdbcDriver: String
  final def getLocalTables(implicit session: profile.backend.Session) = blockingRunOnSession(localTables)
  final def getLocalSequences(implicit session: profile.backend.Session) =
    blockingRunOnSession(localSequences)
  def canGetLocalTables = true
  def localTables: DBIO[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData.getTables("", "", null, null)).map { ts =>
      ts.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
    }
  def localSequences: DBIO[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData.getTables("", "", null, null)).map { ts =>
      ts.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
  def dropUserArtifacts(implicit session: profile.backend.Session) = blockingRunOnSession {
    for {
      tables <- localTables
      sequences <- localSequences
      _ <- DBIO.seq(
        (tables.map(t => sqlu"""drop table if exists #${profile.quoteIdentifier(t)} cascade""") ++
          sequences.map(t => sqlu"""drop sequence if exists #${profile.quoteIdentifier(t)} cascade""")) *
      )
    } yield ()
  }
  override def assertTablesExist(tables: String*): DBIOAction[Unit, NoStream, Effect] =
    DBIO.seq(tables.map(t => sql"""select 1 from #${profile.quoteIdentifier(t)} where 1 < 0""".as[Int]) *)
  override def assertNotTablesExist(tables: String*): DBIOAction[Unit, NoStream, Effect] =
    DBIO.seq(tables.map(t => sql"""select 1 from #${profile.quoteIdentifier(t)} where 1 < 0""".as[Int].failed) *)

  /** Create a single-session database that reuses an existing connection.
    * Used internally by blockingRunOnSession. */
  def createSingleSessionDatabase(implicit session: profile.backend.Session): IODatabase = {
    val wrappedConn = new DelegateConnection(session.conn) {
      override def close(): Unit = ()
    }
    val source = new JdbcDataSource {
      def createConnection(): Connection = wrappedConn
      def close(): Unit = ()
      val maxConnections: Option[Int] = Some(1)
    }
    val dc = DatabaseConfig.forSource[profile.type](profile, source)
    profile.backend.makeDatabase[IO](dc).unsafeRunSync()
  }

  final def blockingRunOnSession[R](a: DBIOAction[R, NoStream, Nothing])
                                   (implicit session: profile.backend.Session): R = {
    val db = createSingleSessionDatabase(session)
    db.run(a).unsafeRunSync()
  }

  protected[this] def runIO[T](io: IO[T]): T = io.unsafeRunSync()
}

abstract class InternalJdbcTestDB(confName: String) extends JdbcTestDB(confName) { self =>
  val url: String
  def createDB(): IODatabase = {
    val dc = DatabaseConfig.forURL[profile.type](profile, url, null, null, null, jdbcDriver, false, ClassLoaderUtil.defaultClassLoader)
    profile.backend.makeDatabase[IO](dc).unsafeRunSync()
  }
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

  override lazy val testClasses: Seq[Class[? <: AsyncTest[? >: Null <: TestDB]]] =
    TestkitConfig.getStrings(config, "testClasses")
      .map(_.map(n => Class.forName(n).asInstanceOf[Class[? <: AsyncTest[? >: Null <: TestDB]]]))
      .getOrElse(super.testClasses)

  def databaseFor(path: String): IODatabase = {
    val dc = DatabaseConfig.forProfileConfig[profile.type](profile, path, config, ClassLoaderUtil.defaultClassLoader)
    profile.backend.makeDatabase[IO](dc).unsafeRunSync()
  }

  override def createDB() = databaseFor("testConn")

  override def cleanUpBefore(): Unit = {
    if(drop.nonEmpty || create.nonEmpty) {
      println("[Creating test database "+this+"]")
      runIO(databaseFor("adminConn").run(
        DBIO.seq((drop ++ create).map(s => sqlu"#$s") *).withPinnedSession
      ))
    }
    if(postCreate.nonEmpty) {
      runIO(createDB().run(
        DBIO.seq(postCreate.map(s => sqlu"#$s") *).withPinnedSession
      ))
    }
  }

  override def cleanUpAfter(): Unit = {
    if(drop.nonEmpty) {
      println("[Dropping test database "+this+"]")
      runIO(databaseFor("adminConn").run(
        DBIO.seq(drop.map(s => sqlu"#$s") *).withPinnedSession
      ))
    }
  }
}

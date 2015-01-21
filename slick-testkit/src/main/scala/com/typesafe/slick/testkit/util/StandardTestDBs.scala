package com.typesafe.slick.testkit.util

import java.io.File
import java.util.logging.{Level, Logger}
import java.sql.SQLException
import scala.concurrent.ExecutionContext
import scala.slick.action._
import scala.slick.driver._
import scala.slick.memory.MemoryDriver
import scala.slick.jdbc.{StaticQuery => Q, ResultSetAction, ResultSetInvoker}
import scala.slick.jdbc.GetResult._
import scala.slick.jdbc.meta.MTable
import org.junit.Assert

object StandardTestDBs {
  lazy val H2Mem = new H2TestDB("h2mem", false) {
    val url = "jdbc:h2:mem:test1"
    override def isPersistent = false
  }

  lazy val H2MemKeepAlive = new H2TestDB("h2mem", true) {
    val url = "jdbc:h2:mem:test1"
  }

  lazy val H2Disk = new H2TestDB("h2disk", false) {
    val dbName = "h2-"+confName
    val url = "jdbc:h2:"+TestkitConfig.testDBPath+"/"+dbName
    override def cleanUpBefore() = TestDB.deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val HsqldbMem = new HsqlDB("hsqldbmem") {
    val dbName = "test1"
    val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
    override def isPersistent = false
  }

  lazy val HsqldbDisk = new HsqlDB("hsqldbdisk") {
    val dbName = "hsqldb-"+confName
    val url = "jdbc:hsqldb:file:"+TestkitConfig.testDBPath+"/"+dbName+";user=SA;password=;shutdown=true;hsqldb.applog=0"
    override def cleanUpBefore() = TestDB.deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val SQLiteMem = new SQLiteTestDB("jdbc:sqlite:file:slick_test?mode=memory&cache=shared", "sqlitemem") {
    override def isPersistent = false
  }

  lazy val SQLiteDisk = {
    val confName = "sqlitedisk"
    val prefix = "sqlite-"+confName
    new SQLiteTestDB("jdbc:sqlite:"+TestkitConfig.testDBPath+"/"+prefix+".db", confName) {
      override def cleanUpBefore() = TestDB.deleteDBFiles(prefix)
    }
  }

  lazy val DerbyMem = new DerbyDB("derbymem") {
    val dbName = "test1"
    val url = "jdbc:derby:memory:"+dbName+";create=true"
    override def cleanUpBefore() = {
      val dropUrl = "jdbc:derby:memory:"+dbName+";drop=true"
      try { profile.backend.Database.forURL(dropUrl, driver = jdbcDriver) withSession(_.conn) }
      catch { case e: SQLException => }
    }
  }

  lazy val DerbyDisk = new DerbyDB("derbydisk") {
    val dbName = "derby-"+confName
    val url = "jdbc:derby:"+TestkitConfig.testDBPath+"/"+dbName+";create=true"
    override def cleanUpBefore() = {
      val dropUrl = "jdbc:derby:"+TestkitConfig.testDBPath+"/"+dbName+";shutdown=true"
      try { profile.backend.Database.forURL(dropUrl, driver = jdbcDriver) withSession(_.conn) }
      catch { case e: SQLException => }
      TestDB.deleteDBFiles(dbName)
    }
  }

  lazy val Postgres = new ExternalJdbcTestDB("postgres") {
    val driver = PostgresDriver
    override def localTables(implicit ec: ExecutionContext): Action[Vector[String]] =
      ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null)).map { ts =>
        ts.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
      }
    override def getLocalSequences(implicit session: profile.Backend#Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.buildColl[List].filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
    override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions
  }

  lazy val MySQL = new ExternalJdbcTestDB("mysql") {
    val driver = MySQLDriver
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val MSAccess = new AccessDB("access")

  lazy val Heap = new RelationalTestDB {
    type Driver = MemoryDriver
    val driver: Driver = MemoryDriver
    val confName: String = "heap"
    def createDB: profile.Backend#Database = profile.backend.Database(ExecutionContext.global)
    def dropUserArtifacts(implicit session: profile.Backend#Session) {
      val db = session.database
      db.getTables.foreach(t => db.dropTable(t.name))
    }
    def assertTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
      val all = session.database.getTables.map(_.name).toSet
      for(t <- tables) {
        if(!all.contains(t)) Assert.fail("Table "+t+" should exist")
      }
    }
    def assertNotTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
      val all = session.database.getTables.map(_.name).toSet
      for(t <- tables) {
        if(all.contains(t)) Assert.fail("Table "+t+" should not exist")
      }
    }
  }
}

abstract class H2TestDB(confName: String, keepAlive: Boolean) extends InternalJdbcTestDB(confName) {
  val driver = H2Driver
  val jdbcDriver = "org.h2.Driver"
  override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions - TestDB.capabilities.jdbcMetaGetClientInfoProperties
  override def createDB(): profile.Backend#Database = database.forURL(url, driver = jdbcDriver, keepAliveConnection = keepAlive)
}

class SQLiteTestDB(dburl: String, confName: String) extends InternalJdbcTestDB(confName) {
  val driver = SQLiteDriver
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  override def localTables(implicit ec: ExecutionContext): Action[Vector[String]] =
    super.localTables.map(_.filter(s => !s.toLowerCase.contains("sqlite_")))
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    for(t <- getLocalTables)
      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)).execute
    for(t <- getLocalSequences)
      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)).execute
  }
}

@deprecated("AccessDriver will be removed when we drop support for Java versions < 8", "2.1")
class AccessDB(confName: String) extends ExternalJdbcTestDB(confName) {
  val driver = AccessDriver
  val dir = new File(TestkitConfig.testDir)
  val dbPath = dir.getAbsolutePath.replace("\\", "/")
  lazy val emptyDBFile = confString("emptyDBFile")
  lazy val testDBFile = confString("testDBFile")

  override def cleanUpBefore() {
    cleanUpAfter()
    TestDB.copy(new File(emptyDBFile), new File(testDBFile))
  }
  override def cleanUpAfter() = TestDB.deleteDBFiles(testDB)
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    session.close()
    cleanUpBefore()
  }
  /* Works in some situations but fails with "Optional feature not implemented" in others */
  override def canGetLocalTables = false
  override def localTables(implicit ec: ExecutionContext): Action[Vector[String]] =
    MTable.getTables.map(_.map(_.name.name).sorted)
  override def capabilities = super.capabilities - TestDB.capabilities.jdbcMeta
}

abstract class DerbyDB(confName: String) extends InternalJdbcTestDB(confName) {
  val driver = DerbyDriver
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  override def localTables(implicit ec: ExecutionContext): Action[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables(null, "APP", null, null)).map { ts =>
      ts.map(_._3).sorted
    }
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    try {
      try { (Q.u+"create table \"__derby_dummy\"(x integer primary key)").execute }
      catch { case ignore: SQLException => }
      val constraints = (Q[(String, String)]+"""
            select c.constraintname, t.tablename
            from sys.sysconstraints c, sys.sysschemas s, sys.systables t
            where c.schemaid = s.schemaid and c.tableid = t.tableid and s.schemaname = 'APP'
                                             """).buildColl[List]
      for((c, t) <- constraints if !c.startsWith("SQL"))
        (Q.u+"alter table "+driver.quoteIdentifier(t)+" drop constraint "+driver.quoteIdentifier(c)).execute
      for(t <- getLocalTables)
        (Q.u+"drop table "+driver.quoteIdentifier(t)).execute
      for(t <- getLocalSequences)
        (Q.u+"drop sequence "+driver.quoteIdentifier(t)).execute
    } catch {
      case e: Exception =>
        println("[Caught Exception while dropping user artifacts in Derby: "+e+"]")
        session.close()
        cleanUpBefore()
    }
  }
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int) {} };
}

abstract class HsqlDB(confName: String) extends InternalJdbcTestDB(confName) {
  val driver = HsqldbDriver
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  override def localTables(implicit ec: ExecutionContext): Action[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables(null, "PUBLIC", null, null)).map { ts =>
      ts.map(_._3).sorted
    }
  override def cleanUpBefore() {
    // Try to turn Hsqldb logging off -- does not work :(
    System.setProperty("hsqldb.reconfig_logging", "false")
    Logger.getLogger("org.hsqldb.persist.Logger").setLevel(Level.OFF)
    Logger.getLogger("org.hsqldb").setLevel(Level.OFF)
    Logger.getLogger("hsqldb").setLevel(Level.OFF)
  }
}

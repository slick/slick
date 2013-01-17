package scala.slick.testutil

import java.io.File
import java.util.logging.{Level, Logger}
import java.sql.SQLException
import scala.slick.driver._
import scala.slick.jdbc.{ResultSetInvoker, StaticQuery => Q}
import scala.slick.jdbc.GetResult._
import scala.slick.jdbc.meta.MTable
import com.typesafe.slick.testkit.util.{ExternalTestDB, TestDB}

object TestDBs {
  def H2Mem(cname: String) = new TestDB("h2mem", H2Driver) {
    val url = "jdbc:h2:mem:test1"
    val jdbcDriver = "org.h2.Driver"
    override def isPersistent = false
    override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
  }

  def H2Disk(cname: String) = new TestDB("h2disk", H2Driver) {
    val dbName = "h2-"+cname
    val url = "jdbc:h2:"+TestDB.testDBPath+"/"+dbName
    val jdbcDriver = "org.h2.Driver"
    override def cleanUpBefore() = TestDB.deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
    override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
  }

  def HsqldbMem(cname: String) = new HsqlDB("hsqldbmem") {
    val dbName = "test1"
    val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
    override def isPersistent = false
  }

  def HsqldbDisk(cname: String) = new HsqlDB("hsqldbdisk") {
    val dbName = "hsqldb-"+cname
    val url = "jdbc:hsqldb:file:"+TestDB.testDBPath+"/"+dbName+";user=SA;password=;shutdown=true;hsqldb.applog=0"
    override def cleanUpBefore() = TestDB.deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  def SQLiteMem(cname: String) = new SQLiteTestDB("jdbc:sqlite::memory:", "sqlitemem") {
    override def isPersistent = false
    override def isShared = false
  }

  def SQLiteDisk(cname: String) = {
    val prefix = "sqlite-"+cname
    new SQLiteTestDB("jdbc:sqlite:"+TestDB.testDBPath+"/"+prefix+".db", "sqlitedisk") {
      override def cleanUpBefore() = TestDB.deleteDBFiles(prefix)
    }
  }

  def DerbyMem(cname: String) = new DerbyDB("derbymem") {
    val dbName = "test1"
    val url = "jdbc:derby:memory:"+dbName+";create=true"
    override def cleanUpBefore() = {
      val dropUrl = "jdbc:derby:memory:"+dbName+";drop=true"
      try { profile.backend.Database.forURL(dropUrl, driver = jdbcDriver) withSession { s:profile.Backend#Session => s.conn } }
      catch { case e: SQLException => }
    }
  }

  def DerbyDisk(cname: String) = new DerbyDB("derbydisk") {
    val dbName = "derby-"+cname
    val url = "jdbc:derby:"+TestDB.testDBPath+"/"+dbName+";create=true"
    override def cleanUpBefore() = {
      val dropUrl = "jdbc:derby:"+TestDB.testDBPath+"/"+dbName+";shutdown=true"
      try { profile.backend.Database.forURL(dropUrl, driver = jdbcDriver) withSession { s:profile.Backend#Session => s.conn } }
      catch { case e: SQLException => }
      TestDB.deleteDBFiles(dbName)
    }
  }

  def Postgres(cname: String) = new ExternalTestDB("postgres", PostgresDriver) {
    override def getLocalTables(implicit session: profile.Backend#Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.list.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
    }
    override def getLocalSequences(implicit session: profile.Backend#Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.list.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
    override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
  }

  def MySQL(cname: String) = new ExternalTestDB("mysql", MySQLDriver) {
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
    /*override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      val constraints = (Q[(String, String)]+"""
          select distinct constraint_name, table_name
          from information_schema.key_column_usage
          where referenced_table_name is not null
        """).list
      println("###### "+constraints)
      for((c, t) <- constraints if !c.startsWith("SQL"))
        (Q.u+"alter table "+driver.quoteIdentifier(t)+" drop foreign key "+driver.quoteIdentifier(c)).execute()
      for(t <- getLocalTables)
        (Q.u+"drop table if exists "+driver.quoteIdentifier(t)+" cascade").execute()
      for(t <- getLocalSequences)
        (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)+" cascade").execute()
    }*/
    override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
  }

  def SQLServerJTDS(cname: String) = new SQLServerDB("sqlserver")

  def SQLServerSQLJDBC(cname: String) = new SQLServerDB("sqlserver-jdbc")

  def MSAccess(cname: String) = new AccessDB("access")
}

class SQLiteTestDB(dburl: String, confName: String) extends TestDB(confName, SQLiteDriver) {
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  override def getLocalTables(implicit session: profile.Backend#Session) =
    super.getLocalTables.filter(s => !s.toLowerCase.contains("sqlite_"))
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    for(t <- getLocalTables)
      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)).execute()
    for(t <- getLocalSequences)
      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)).execute()
  }
  override lazy val capabilities = driver.capabilities + TestDB.plainSql
}

class AccessDB(confName: String) extends TestDB(confName, AccessDriver) {
  val jdbcDriver = TestDB.get(confName, "driver").orNull
  def dbName = TestDB.get(confName, "testDB").get
  val dir = new File(TestDB.testDBDir)
  val dbPath = dir.getAbsolutePath.replace("\\", "/")
  lazy val emptyDBFile = TestDB.get(confName, "emptyDBFile").get
    .replace("[DB]", dbName).replace("[DBPATH]", dbPath)
  lazy val testDBFile = TestDB.get(confName, "testDBFile").get
    .replace("[DB]", dbName).replace("[DBPATH]", dbPath)
  lazy val url = TestDB.get(confName, "url").getOrElse("")
    .replace("[DB]", dbName).replace("[DBPATH]", dbPath)

  override def isEnabled = TestDB.isExternalEnabled(confName)
  override def createDB() = profile.backend.Database.forURL(url, driver = jdbcDriver)
  override def cleanUpBefore() {
    cleanUpAfter()
    TestDB.copy(new File(emptyDBFile), new File(testDBFile))
  }
  override def cleanUpAfter() = TestDB.deleteDBFiles(dbName)
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    session.close()
    cleanUpBefore()
  }
  /* Works in some situations but fails with "Optional feature not implemented" in others */
  override def canGetLocalTables = false
  override def getLocalTables(implicit session: profile.Backend#Session) =
    MTable.getTables.list.map(_.name.name).sorted
  override lazy val capabilities = driver.capabilities + TestDB.plainSql
}

abstract class DerbyDB(confName: String) extends TestDB(confName, DerbyDriver) {
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  override def getLocalTables(implicit session: profile.Backend#Session): List[String] = {
    val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables(null, "APP", null, null))
    tables.list.map(_._3).sorted
  }
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    try {
      try { (Q.u+"create table \"__derby_dummy\"(x integer primary key)").execute }
      catch { case ignore: SQLException => }
      val constraints = (Q[(String, String)]+"""
            select c.constraintname, t.tablename
            from sys.sysconstraints c, sys.sysschemas s, sys.systables t
            where c.schemaid = s.schemaid and c.tableid = t.tableid and s.schemaname = 'APP'
                                             """).list
      for((c, t) <- constraints if !c.startsWith("SQL"))
        (Q.u+"alter table "+driver.quoteIdentifier(t)+" drop constraint "+driver.quoteIdentifier(c)).execute()
      for(t <- getLocalTables)
        (Q.u+"drop table "+driver.quoteIdentifier(t)).execute()
      for(t <- getLocalSequences)
        (Q.u+"drop sequence "+driver.quoteIdentifier(t)).execute()
    } catch {
      case e: Exception =>
        println("[Caught Exception while dropping user artifacts in Derby: "+e+"]")
        session.close()
        cleanUpBefore()
    }
  }
  override lazy val capabilities = driver.capabilities + TestDB.plainSql
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int) {} };
}

abstract class HsqlDB(confName: String) extends TestDB(confName, HsqldbDriver) {
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  override def getLocalTables(implicit session: profile.Backend#Session): List[String] = {
    val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables(null, "PUBLIC", null, null))
    tables.list.map(_._3).sorted
  }
  override def cleanUpBefore() {
    // Try to turn Hsqldb logging off -- does not work :(
    System.setProperty("hsqldb.reconfig_logging", "false")
    Logger.getLogger("org.hsqldb.persist.Logger").setLevel(Level.OFF)
    Logger.getLogger("org.hsqldb").setLevel(Level.OFF)
    Logger.getLogger("hsqldb").setLevel(Level.OFF)
  }
  override lazy val capabilities = driver.capabilities + TestDB.plainSql
}

class SQLServerDB(confName: String) extends ExternalTestDB(confName, SQLServerDriver) {
  val defaultSchema = TestDB.get(confName, "defaultSchema").getOrElse("")
  override def getLocalTables(implicit session: profile.Backend#Session): List[String] = {
    val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables(dbName, defaultSchema, null, null))
    tables.list.map(_._3).sorted
  }
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    val constraints = (Q[(String, String)]+"""
      select constraint_name, table_name
      from information_schema.table_constraints
      where constraint_type = 'FOREIGN KEY'
      """).list
    for((c, t) <- constraints if !c.startsWith("SQL"))
    (Q.u+"alter table "+driver.quoteIdentifier(t)+" drop constraint "+driver.quoteIdentifier(c)).execute()
    for(t <- getLocalTables)
    (Q.u+"drop table "+driver.quoteIdentifier(t)).execute()
  }
  override lazy val capabilities = driver.capabilities + TestDB.plainSql
}

package scala.slick.testutil

import java.util.Properties
import java.sql.SQLException
import scala.slick.session._
import scala.slick.jdbc.{StaticQuery => Q, ResultSetInvoker}
import scala.slick.jdbc.GetResult._
import java.util.zip.GZIPInputStream
import java.io._
import org.junit.Assert
import scala.slick.jdbc.meta.MTable
import scala.slick.driver._
import java.util.logging.{Level, Logger}

object TestDBOptions {
  val testDBDir = dbProps.getProperty("testDir", "test-dbs")
  def testDBPath = {
    val f = new File(testDBDir)
    val s = f.getPath().replace('\\', '/')
    if(f.isAbsolute) s else "./" + s
  }
  lazy val dbProps = {
    val p = new Properties
    val f = new File("test-dbs", "databases.properties")
    if(f.isFile) {
      val in = new FileInputStream(f)
      try { p.load(in) } finally { in.close() }
    }
    p
  }
  lazy val testDBs = Option(dbProps.getProperty("testDBs")).map(_.split(',').map(_.trim).toSet)
  def isInternalEnabled(db: String) = testDBs.map(_.contains(db)).getOrElse(true)
  def isExternalEnabled(db: String) = isInternalEnabled(db) && "true" == dbProps.getProperty(db+".enabled")
  def get(db: String, o: String) = Option(dbProps.getProperty(db+"."+o))
}

abstract class TestDB(val confName: String) {
  override def toString = url
  val url: String
  val jdbcDriver: String
  val driver: ExtendedDriver
  lazy val profile: ExtendedProfile = driver
  def dbName = ""
  def userName = ""
  def createDB() = Database.forURL(url, driver = jdbcDriver)
  def cleanUpBefore() = cleanUp()
  def cleanUpAfter() = cleanUp()
  def cleanUp() {}
  def deleteDBFiles(prefix: String) {
    def deleteRec(f: File): Boolean = {
      if(f.isDirectory()) f.listFiles.forall(deleteRec _) && f.delete()
      else f.delete()
    }
    val dir = new File(TestDBOptions.testDBDir)
    if(!dir.isDirectory) throw new IOException("Directory "+TestDBOptions.testDBDir+" not found")
    for(f <- dir.listFiles if f.getName startsWith prefix) {
      val p = TestDBOptions.testDBDir+"/"+f.getName
      if(deleteRec(f)) println("[Deleted database file "+p+"]")
      else throw new IOException("Couldn't delete database file "+p)
    }
  }
  def isEnabled = TestDBOptions.isInternalEnabled(confName)
  def isPersistent = true
  def isShared = true
  def getLocalTables(implicit session: Session) = {
    val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "", null, null))
    tables.list.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
  }
  def getLocalSequences(implicit session: Session) = {
    val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "", null, null))
    tables.list.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
  }
  def dropUserArtifacts(implicit session: Session) = {
    for(t <- getLocalTables)
      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)+" cascade").execute()
    for(t <- getLocalSequences)
      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)+" cascade").execute()
  }
  def assertTablesExist(tables: String*)(implicit session: Session) {
    for(t <- tables) {
      try ((Q[Int]+"select 1 from "+driver.quoteIdentifier(t)+" where 1 < 0").list) catch { case _: Exception =>
        Assert.fail("Table "+t+" should exist")
      }
    }
  }
  def assertNotTablesExist(tables: String*)(implicit session: Session) {
    for(t <- tables) {
      try {
        (Q[Int]+"select 1 from "+driver.quoteIdentifier(t)+" where 1 < 0").list
        Assert.fail("Table "+t+" should not exist")
      } catch { case _: Exception => }
    }
  }
  def assertUnquotedTablesExist(tables: String*)(implicit session: Session) {
    for(t <- tables) {
      try ((Q[Int]+"select 1 from "+t+" where 1 < 0").list) catch { case _: Exception =>
        Assert.fail("Table "+t+" should exist")
      }
    }
  }
  def assertNotUnquotedTablesExist(tables: String*)(implicit session: Session) {
    for(t <- tables) {
      try {
        (Q[Int]+"select 1 from "+t+" where 1 < 0").list
        Assert.fail("Table "+t+" should not exist")
      } catch { case _: Exception => }
    }
  }
  def copy(src: File, dest: File) {
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
  def canGetLocalTables = true
}

class SQLiteTestDB(dburl: String, confName: String) extends TestDB(confName) {
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  val driver = SQLiteDriver
  override def getLocalTables(implicit session: Session) =
    super.getLocalTables.filter(s => !s.toLowerCase.contains("sqlite_"))
  override def dropUserArtifacts(implicit session: Session) = {
    for(t <- getLocalTables)
      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)).execute()
    for(t <- getLocalSequences)
      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)).execute()
  }
}

class ExternalTestDB(confName: String, val driver: ExtendedDriver) extends TestDB(confName) {
  val jdbcDriver = TestDBOptions.get(confName, "driver").orNull
  val urlTemplate = TestDBOptions.get(confName, "url").getOrElse("")
  override def dbName = TestDBOptions.get(confName, "testDB").getOrElse("")
  val url = urlTemplate.replace("[DB]", dbName)
  val configuredUserName = TestDBOptions.get(confName, "user").orNull
  val password = TestDBOptions.get(confName, "password").orNull
  override def userName = TestDBOptions.get(confName, "user").orNull

  val adminDBURL = urlTemplate.replace("[DB]", TestDBOptions.get(confName, "adminDB").getOrElse(""))
  val create = TestDBOptions.get(confName, "create").getOrElse("").replace("[DB]", dbName).replace("[DBPATH]", new File(TestDBOptions.testDBDir).getAbsolutePath)
  val drop = TestDBOptions.get(confName, "drop").getOrElse("").replace("[DB]", dbName).replace("[DBPATH]", new File(TestDBOptions.testDBDir).getAbsolutePath)

  override def isEnabled = TestDBOptions.isExternalEnabled(confName)

  override def createDB() = Database.forURL(url, driver = jdbcDriver, user = configuredUserName, password = password)

  override def cleanUpBefore() {
    if(drop.length > 0 || create.length > 0) {
      println("[Creating test database "+this+"]")
      Database.forURL(adminDBURL, driver = jdbcDriver, user = configuredUserName, password = password) withSession { implicit s: Session =>
        (Q.u + drop).execute
        (Q.u + create).execute
      }
    }
  }

  override def cleanUpAfter() {
    if(drop.length > 0) {
      println("[Dropping test database "+this+"]")
      Database.forURL(adminDBURL, driver = jdbcDriver, user = configuredUserName, password = password) withSession { implicit s: Session =>
        (Q.u + drop).execute
      }
    }
  }
}

class AccessDB(confName: String, val driver: ExtendedDriver) extends TestDB(confName) {
  val jdbcDriver = TestDBOptions.get(confName, "driver").orNull
  override def dbName = TestDBOptions.get(confName, "testDB").getOrElse(super.dbName)
  val dir = new File(TestDBOptions.testDBDir)
  val dbPath = dir.getAbsolutePath.replace("\\", "/")
  lazy val emptyDBFile = TestDBOptions.get(confName, "emptyDBFile").get
    .replace("[DB]", dbName).replace("[DBPATH]", dbPath)
  lazy val testDBFile = TestDBOptions.get(confName, "testDBFile").get
    .replace("[DB]", dbName).replace("[DBPATH]", dbPath)
  lazy val url = TestDBOptions.get(confName, "url").getOrElse("")
    .replace("[DB]", dbName).replace("[DBPATH]", dbPath)

  override def isEnabled = TestDBOptions.isExternalEnabled(confName)
  override def createDB() = Database.forURL(url, driver = jdbcDriver)
  override def cleanUpBefore() {
    cleanUpAfter()
    copy(new File(emptyDBFile), new File(testDBFile))
  }
  override def cleanUpAfter() = deleteDBFiles(dbName)
  override def dropUserArtifacts(implicit session: Session) = cleanUpBefore()
  /* Works in some situations but fails with "Optional feature not implemented" in others */
  override def canGetLocalTables = false
  override def getLocalTables(implicit session: Session) =
    MTable.getTables.list.map(_.name.name).sorted
}

abstract class DerbyDB(confName: String) extends TestDB(confName) {
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  val driver = DerbyDriver
  override def userName = "APP"
  override def getLocalTables(implicit session: Session): List[String] = {
    val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables(null, "APP", null, null))
    tables.list.map(_._3).sorted
  }
  override def dropUserArtifacts(implicit session: Session) = {
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
  }
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int) {} };
}

abstract class HsqlDB(confName: String) extends TestDB(confName) {
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  val driver = HsqldbDriver
  override def getLocalTables(implicit session: Session): List[String] = {
    val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables(null, "PUBLIC", null, null))
    tables.list.map(_._3).sorted
  }
  override def userName = "sa"
  override def cleanUpBefore() {
    // Try to turn Hsqldb logging off -- does not work :(
    System.setProperty("hsqldb.reconfig_logging", "false")
    Logger.getLogger("org.hsqldb.persist.Logger").setLevel(Level.OFF)
    Logger.getLogger("org.hsqldb").setLevel(Level.OFF)
    Logger.getLogger("hsqldb").setLevel(Level.OFF)
    super.cleanUpBefore()
  }
}

object TestDB {
  type TestDBSpec = (String => TestDB)

  def H2Mem(cname: String) = new TestDB("h2mem") {
    val url = "jdbc:h2:mem:test1"
    val jdbcDriver = "org.h2.Driver"
    val driver = H2Driver
    override val dbName = "test1"
    override def isPersistent = false
  }

  def H2Disk(cname: String) = new TestDB("h2disk") {
    override val dbName = "h2-"+cname
    val url = "jdbc:h2:"+TestDBOptions.testDBPath+"/"+dbName
    val jdbcDriver = "org.h2.Driver"
    val driver = H2Driver
    override def cleanUp() = deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: Session) = cleanUp()
  }

  def HsqldbMem(cname: String) = new HsqlDB("hsqldbmem") {
    override val dbName = "test1"
    val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
    override def isPersistent = false
  }

  def HsqldbDisk(cname: String) = new HsqlDB("hsqldbdisk") {
    override val dbName = "hsqldb-"+cname
    val url = "jdbc:hsqldb:file:"+TestDBOptions.testDBPath+"/"+dbName+";user=SA;password=;shutdown=true;hsqldb.applog=0"
    override def cleanUp() = deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: Session) = cleanUp()
  }

  def SQLiteMem(cname: String) = new SQLiteTestDB("jdbc:sqlite::memory:", "sqlitemem") {
    override def isPersistent = false
    override def isShared = false
  }

  def SQLiteDisk(cname: String) = {
    val prefix = "sqlite-"+cname
    new SQLiteTestDB("jdbc:sqlite:"+TestDBOptions.testDBPath+"/"+prefix+".db", "sqlitedisk") {
      override val dbName = prefix
      override def cleanUp() = deleteDBFiles(prefix)
    }
  }

  def DerbyMem(cname: String) = new DerbyDB("derbymem") {
    override val dbName = "test1"
    val url = "jdbc:derby:memory:"+dbName+";create=true"
    override def cleanUp() = {
      val dropUrl = "jdbc:derby:memory:"+dbName+";drop=true"
      try { Database.forURL(dropUrl, driver = jdbcDriver) withSession { s:Session => s.conn } }
      catch { case e: SQLException => }
    }
  }

  def DerbyDisk(cname: String) = new DerbyDB("derbydisk") {
    override val dbName = "derby-"+cname
    val url = "jdbc:derby:"+TestDBOptions.testDBPath+"/"+dbName+";create=true"
    override def cleanUp() = {
      val dropUrl = "jdbc:derby:"+TestDBOptions.testDBPath+"/"+dbName+";shutdown=true"
      try { Database.forURL(dropUrl, driver = jdbcDriver) withSession { s:Session => s.conn } }
      catch { case e: SQLException => }
      deleteDBFiles(dbName)
    }
  }

  def Postgres(cname: String) = new ExternalTestDB("postgres", PostgresDriver) {
    override def getLocalTables(implicit session: Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.list.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
    }
    override def getLocalSequences(implicit session: Session) = {
      val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null))
      tables.list.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
    }
  }

  def MySQL(cname: String) = new ExternalTestDB("mysql", MySQLDriver) {
    override def userName = super.userName + "@localhost"
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: Session) = {
      cleanUpAfter()
      cleanUpBefore()
    }
    /*override def dropUserArtifacts(implicit session: Session) = {
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
  }

  def SQLServer(cname: String) = new ExternalTestDB("sqlserver", SQLServerDriver) {
    val defaultSchema = TestDBOptions.get(confName, "defaultSchema").getOrElse("")
    override def getLocalTables(implicit session: Session): List[String] = {
      val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables(dbName, defaultSchema, null, null))
      tables.list.map(_._3).sorted
    }
    override def dropUserArtifacts(implicit session: Session) = {
      val constraints = (Q[(String, String)]+"""
          select constraint_name, table_name
          from information_schema.table_constraints
          where constraint_type = 'FOREIGN KEY'
        """).list
      println("##### "+constraints)
      for((c, t) <- constraints if !c.startsWith("SQL"))
        (Q.u+"alter table "+driver.quoteIdentifier(t)+" drop constraint "+driver.quoteIdentifier(c)).execute()
      for(t <- getLocalTables)
        (Q.u+"drop table "+driver.quoteIdentifier(t)).execute()
    }
  }

  def MSAccess(cname: String) = new AccessDB("access", AccessDriver)
}

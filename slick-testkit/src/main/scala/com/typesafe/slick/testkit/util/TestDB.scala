package com.typesafe.slick.testkit.util

import java.util.Properties
import scala.slick.jdbc.{StaticQuery => Q, ResultSetInvoker}
import scala.slick.jdbc.GetResult._
import java.util.zip.GZIPInputStream
import java.io._
import org.junit.Assert
import scala.slick.driver._
import java.net.{URL, URLClassLoader}
import java.sql.Driver
import scala.collection.mutable
import scala.slick.profile.Capability

object TestDB {
  type TestDBSpec = (String => TestDB)

  /** Marks a driver which is specially supported by the test kit for plain SQL queries */
  val plainSql = new Capability("test.plainSql")
  /** Marks a driver which is specially supported by the test kit for plain SQL wide result set queries */
  val plainSqlWide = new Capability("test.plainSqlWide")

  val testDBDir = dbProps.getProperty("testDir", "test-dbs")
  def testDBPath = {
    val f = new File(testDBDir)
    val s = f.getPath().replace('\\', '/')
    if(f.isAbsolute) s else "./" + s
  }
  private lazy val dbProps = {
    val p = new Properties
    val f = new File("test-dbs", "databases.properties")
    if(f.isFile) {
      val in = new FileInputStream(f)
      try { p.load(in) } finally { in.close() }
    }
    p
  }
  private lazy val testDBs = Option(dbProps.getProperty("testDBs")).map(_.split(',').map(_.trim).toSet)
  def isInternalEnabled(db: String) = testDBs.map(_.contains(db)).getOrElse(true)
  def isExternalEnabled(db: String) = isInternalEnabled(db) && "true" == dbProps.getProperty(db+".enabled")
  def get(db: String, o: String) = Option(dbProps.getProperty(db+"."+o))

  def getMulti(db: String, key: String): Seq[String] = get(db, key) match {
    case Some(s) => Seq(s)
    case None =>
      Iterator.from(1).map(i => get(db, key+"."+i)).takeWhile(_.isDefined).toSeq.flatten
  }

  /** Copy a file, expanding it if the source name ends with .gz */
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

  /** Delete files in the testDB directory */
  def deleteDBFiles(prefix: String) {
    assert(!prefix.isEmpty, "prefix must not be empty")
    def deleteRec(f: File): Boolean = {
      if(f.isDirectory()) f.listFiles.forall(deleteRec _) && f.delete()
      else f.delete()
    }
    val dir = new File(TestDB.testDBDir)
    if(!dir.isDirectory) throw new IOException("Directory "+TestDB.testDBDir+" not found")
    for(f <- dir.listFiles if f.getName startsWith prefix) {
      val p = TestDB.testDBDir+"/"+f.getName
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
abstract class TestDB(final val confName: String, final val driver: JdbcDriver) {
  final val profile: JdbcProfile = driver
  protected val Database = profile.backend.Database

  override def toString = url
  val url: String
  val jdbcDriver: String
  def createDB() = Database.forURL(url, driver = jdbcDriver)
  def cleanUpBefore() {}
  def cleanUpAfter() = cleanUpBefore()
  def isEnabled = TestDB.isInternalEnabled(confName)
  def isPersistent = true
  def isShared = true
  def getLocalTables(implicit session: profile.Backend#Session) = {
    val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "", null, null))
    tables.list.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
  }
  def getLocalSequences(implicit session: profile.Backend#Session) = {
    val tables = ResultSetInvoker[(String,String,String, String)](_.conn.getMetaData().getTables("", "", null, null))
    tables.list.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
  }
  def dropUserArtifacts(implicit session: profile.Backend#Session) = {
    for(t <- getLocalTables)
      (Q.u+"drop table if exists "+driver.quoteIdentifier(t)+" cascade").execute()
    for(t <- getLocalSequences)
      (Q.u+"drop sequence if exists "+driver.quoteIdentifier(t)+" cascade").execute()
  }
  def assertTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
    for(t <- tables) {
      try ((Q[Int]+"select 1 from "+driver.quoteIdentifier(t)+" where 1 < 0").list) catch { case _: Exception =>
        Assert.fail("Table "+t+" should exist")
      }
    }
  }
  def assertNotTablesExist(tables: String*)(implicit session: profile.Backend#Session) {
    for(t <- tables) {
      try {
        (Q[Int]+"select 1 from "+driver.quoteIdentifier(t)+" where 1 < 0").list
        Assert.fail("Table "+t+" should not exist")
      } catch { case _: Exception => }
    }
  }
  def canGetLocalTables = true
  lazy val capabilities = driver.capabilities
}

class ExternalTestDB(confName: String, driver: JdbcDriver) extends TestDB(confName, driver) {
  val jdbcDriver = TestDB.get(confName, "driver").orNull
  val urlTemplate = TestDB.get(confName, "url").getOrElse("")
  val dbPath = new File(TestDB.testDBDir).getAbsolutePath
  val dbName = TestDB.get(confName, "testDB").getOrElse("").replace("[DBPATH]", dbPath)
  val password = TestDB.get(confName, "password").orNull
  val user = TestDB.get(confName, "user").orNull
  val adminUser = TestDB.get(confName, "adminUser").getOrElse(user)
  val adminPassword = TestDB.get(confName, "adminPassword").getOrElse(password)
  lazy val url = replaceVars(urlTemplate)

  lazy val adminDB = TestDB.get(confName, "adminDB").getOrElse("").replace("[DBPATH]", dbPath)
  lazy val adminDBURL = replaceVars(urlTemplate.replace("[DB]", adminDB))
  lazy val create = TestDB.getMulti(confName, "create").map(replaceVars)
  lazy val postCreate = TestDB.getMulti(confName, "postCreate").map(replaceVars)
  lazy val drop = TestDB.getMulti(confName, "drop").map(replaceVars)

  def replaceVars(s: String): String =
    s.replace("[DB]", dbName).replace("[DBPATH]", dbPath).
      replace("[USER]", user).replace("[PASSWORD]", password)

  override def isEnabled = TestDB.isExternalEnabled(confName)

  def databaseFor(url: String, user: String, password: String, prop: Map[String, String] = null) = loadCustomDriver() match {
    case Some(dr) => Database.forDriver(dr, url, user = user, password = password, prop = TestDB.mapToProps(prop))
    case None => Database.forURL(url, user = user, password = password, driver = jdbcDriver, prop = TestDB.mapToProps(prop))
  }

  override def createDB() = databaseFor(url, user, password)

  override def cleanUpBefore() {
    if(!drop.isEmpty || !create.isEmpty) {
      println("[Creating test database "+this+"]")
      databaseFor(adminDBURL, adminUser, adminPassword) withSession { implicit session: profile.Backend#Session =>
        for(s <- drop) (Q.u + s).execute
        for(s <- create) (Q.u + s).execute
      }
    }
    if(!postCreate.isEmpty) {
      createDB() withSession { implicit session: profile.Backend#Session =>
        for(s <- postCreate) (Q.u + s).execute
      }
    }
  }

  override def cleanUpAfter() {
    if(!drop.isEmpty) {
      println("[Dropping test database "+this+"]")
      databaseFor(adminDBURL, adminUser, adminPassword) withSession { implicit session: profile.Backend#Session =>
        for(s <- drop) (Q.u + s).execute
      }
    }
  }

  def loadCustomDriver() = TestDB.get(confName, "driverJar").map { jar =>
    ExternalTestDB.getCustomDriver(jar, jdbcDriver)
  }
}

object ExternalTestDB {
  // A cache for custom drivers to avoid excessive reloading and memory leaks
  private[this] val driverCache = new mutable.HashMap[(String, String), Driver]()

  def getCustomDriver(url: String, driverClass: String): Driver = synchronized {
    driverCache.getOrElseUpdate((url, driverClass),
      new URLClassLoader(Array(new URL(url)), getClass.getClassLoader).loadClass(driverClass).newInstance.asInstanceOf[Driver]
    )
  }
}
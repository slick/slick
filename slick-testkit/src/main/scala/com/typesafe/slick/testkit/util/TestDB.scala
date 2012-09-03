package com.typesafe.slick.testkit.util

import java.util.Properties
import scala.slick.session._
import scala.slick.jdbc.{StaticQuery => Q, ResultSetInvoker}
import scala.slick.jdbc.GetResult._
import java.util.zip.GZIPInputStream
import java.io._
import org.junit.Assert
import scala.slick.driver._
import java.net.{URL, URLClassLoader}
import java.sql.Driver

object TestDB {
  type TestDBSpec = (String => TestDB)

  /** Marks a driver which is specially supported by the test kit for plain SQL queries */
  val plainSql = new Capability("test.plainSql")

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
}

/**
 * Describes a database against which you can run TestKit tests. It includes
 * features such as reading the configuration file, setting up a DB connection,
 * removing DB files left over by a test run, etc.
 */
abstract class TestDB(final val confName: String, final val driver: ExtendedDriver) {
  final val profile: ExtendedProfile = driver

  override def toString = url
  val url: String
  val jdbcDriver: String
  def dbName = ""
  def createDB() = Database.forURL(url, driver = jdbcDriver)
  def cleanUpBefore() {}
  def cleanUpAfter() = cleanUpBefore()
  def isEnabled = TestDB.isInternalEnabled(confName)
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
  def canGetLocalTables = true
  lazy val capabilities = driver.capabilities
}

class ExternalTestDB(confName: String, driver: ExtendedDriver) extends TestDB(confName, driver) {
  val jdbcDriver = TestDB.get(confName, "driver").orNull
  val urlTemplate = TestDB.get(confName, "url").getOrElse("")
  override def dbName = TestDB.get(confName, "testDB").getOrElse("")
  val url = urlTemplate.replace("[DB]", dbName)
  val password = TestDB.get(confName, "password").orNull
  val user = TestDB.get(confName, "user").orNull
  val adminUser = TestDB.get(confName, "adminUser").getOrElse(user)
  val adminPassword = TestDB.get(confName, "adminPassword").getOrElse(password)

  val adminDBURL = urlTemplate.replace("[DB]", TestDB.get(confName, "adminDB").getOrElse(""))
  val create = replaceVars(TestDB.get(confName, "create").getOrElse(""))
  val drop = replaceVars(TestDB.get(confName, "drop").getOrElse(""))

  def replaceVars(s: String) =
    s.replace("[DB]", dbName).replace("[DBPATH]", new File(TestDB.testDBDir).getAbsolutePath).
      replace("[USER]", user).replace("[PASSWORD]", password)

  override def isEnabled = TestDB.isExternalEnabled(confName)

  def databaseFor(url: String, user: String, password: String) = loadCustomDriver() match {
    case Some(dr) => Database.forDriver(dr, url, user = user, password = password)
    case None => Database.forURL(url, user = user, password = password, driver = jdbcDriver)
  }

  override def createDB() = databaseFor(url, user, password)

  override def cleanUpBefore() {
    if(drop.length > 0 || create.length > 0) {
      println("[Creating test database "+this+"]")
      databaseFor(adminDBURL, adminUser, adminPassword) withSession { implicit s: Session =>
        if(drop != "") (Q.u + drop).execute
        if(create != "") (Q.u + create).execute
      }
    }
  }

  override def cleanUpAfter() {
    if(drop.length > 0) {
      println("[Dropping test database "+this+"]")
      databaseFor(adminDBURL, adminUser, adminPassword) withSession { implicit s: Session =>
        if(drop != "") (Q.u + drop).execute
      }
    }
  }

  def loadCustomDriver() = TestDB.get(confName, "driverJar").map { jar =>
    new URLClassLoader(Array(new URL(jar)), getClass.getClassLoader).loadClass(jdbcDriver).newInstance.asInstanceOf[Driver]
  }
}

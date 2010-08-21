package com.novocode.squery.test.util

import scala.collection.JavaConversions._
import java.io.{File, IOException, FileInputStream}
import java.util.Properties
import com.novocode.squery.combinator.extended.{ExtendedProfile, H2Driver, SQLiteDriver, PostgresDriver}
import com.novocode.squery.ResultSetInvoker
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession
import com.novocode.squery.simple._
import com.novocode.squery.simple.StaticQueryBase._
import com.novocode.squery.simple.Implicit._

object TestDBOptions {
  val testDBDir = "test-dbs"
  lazy val dbProps = {
    val p = new Properties
    val in = new FileInputStream(new File(testDBDir, "databases.properties"))
    try { p.load(in) } finally { in.close() }
    p
  }
  def isEnabled(db: String) = "true" == dbProps.getProperty(db+".enabled")
  def get(db: String, o: String) = Option(dbProps.getProperty(db+"."+o))
}

class TestDB(val url: String, val jdbcDriver: String, val driver: ExtendedProfile) {
  override def toString = url
  def createDB() = Database.forURL(url, driver = jdbcDriver)
  def cleanUpBefore() = cleanUp()
  def cleanUpAfter() = cleanUp()
  def cleanUp() {}
  def deleteDBFiles(prefix: String) {
    val dir = new File(TestDBOptions.testDBDir)
    if(!dir.isDirectory) throw new IOException("Directory "+TestDBOptions.testDBDir+" not found")
    for(f <- dir.listFiles if f.getName startsWith prefix) {
      val p = TestDBOptions.testDBDir+"/"+f.getName
      if(f.delete) println("[Deleted database file "+p+"]")
      else throw new IOException("Couldn't delete database file "+p)
    }
  }
  def isEnabled = true
  def getLocalTables(implicit session: Session): List[String] = {
    val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables("", "", null, null))
    tables.list(())(session).map(_._3.toLowerCase)
  }
  def dbName = ""
  def userName = ""
}

class SQLiteTestDB(url: String) extends TestDB(url, "org.sqlite.JDBC", SQLiteDriver) {
  override def getLocalTables(implicit session: Session) =
    super.getLocalTables(session).filter(s => !s.contains("sqlite_")).sortBy(identity)
}

object TestDB {
  type TestDBSpec = (DBTestObject => TestDB)

  def H2Mem(to: DBTestObject) = new TestDB("jdbc:h2:mem:test1", "org.h2.Driver", H2Driver) {
    override def dbName = "test1"
  }

  def H2Disk(to: DBTestObject) = {
    val prefix = "h2-"+to.testClassName
    new TestDB("jdbc:h2:./"+TestDBOptions.testDBDir+"/"+prefix, "org.h2.Driver", H2Driver) {
      override def cleanUp() = deleteDBFiles(prefix)
      override def dbName = prefix
    }
  }

  def SQLiteMem(to: DBTestObject) = new SQLiteTestDB("jdbc:sqlite::memory:") {
    override def dbName = ":memory:"
  }

  def SQLiteDisk(to: DBTestObject) = {
    val prefix = "sqlite-"+to.testClassName
    new SQLiteTestDB("jdbc:sqlite:./"+TestDBOptions.testDBDir+"/"+prefix+".db") {
      override def cleanUp() = deleteDBFiles(prefix)
      override def dbName = prefix
    }
  }

  def Postgres(to: DBTestObject) = {
    val confName = "postgres"
    lazy val user = TestDBOptions.get(confName, "user").getOrElse("postgres")
    lazy val urlTemplate = TestDBOptions.get(confName, "url").getOrElse("jdbc:postgresql:[DB]?user=[USER]").replace("[USER]", user)
    lazy val adminDBURL = urlTemplate.replace("[DB]", TestDBOptions.get(confName, "adminDB").getOrElse("postgres"))
    lazy val testDB = TestDBOptions.get(confName, "testDB").getOrElse("scala-query-test")
    lazy val testDBURL = urlTemplate.replace("[DB]", testDB)
    lazy val create = TestDBOptions.get(confName, "create").getOrElse("CREATE DATABASE \"[DB]\"").replace("[DB]", testDB)
    lazy val drop = TestDBOptions.get(confName, "drop").getOrElse("DROP DATABASE IF EXISTS \"[DB]\"").replace("[DB]", testDB)
    new TestDB(testDBURL, "org.postgresql.Driver", PostgresDriver) {
      override def dbName = testDB
      override def userName = user
      override def isEnabled = TestDBOptions.isEnabled(confName)
      override def toString = {
        val i = url.indexOf('?')
        if(i == -1) url else url.substring(0, i)
      }
      override def cleanUpBefore() {
        println("[Creating test database "+this+"]")
        Database.forURL(adminDBURL, driver = jdbcDriver) withSession {
          updateNA(drop).execute
          updateNA(create).execute
        }
      }
      override def cleanUpAfter() {
        println("[Dropping test database "+this+"]")
        Database.forURL(adminDBURL, driver = jdbcDriver) withSession { updateNA(drop).execute }
      }
      override def getLocalTables(implicit session: Session) = {
        val tables = ResultSetInvoker[(String,String,String)](_.conn.getMetaData().getTables("", "public", null, null))
        tables.list(())(session).map(_._3.toLowerCase).filter(s => !s.endsWith("_pkey") && !s.endsWith("_id_seq"))
      }
    }
  }
}

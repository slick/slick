package com.novocode.squery.test.util

import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import java.io.{File, IOException}
import org.junit.{Before, After}
import org.junit.runner.{JUnitCore, RunWith}
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import com.novocode.squery.session._
import com.novocode.squery.combinator.extended.{ExtendedProfile, H2Driver}

@RunWith(classOf[Parameterized])
class DBTest(testDB: TestDB) {
  println("[Using test database "+testDB+"]")
  lazy val db = testDB.createDB()
  private[this] var sessionCreated = false
  lazy val session = { sessionCreated = true; db.createSession() }
  @Before def beforeDBTest = testDB.cleanUp()
  @After def afterDBTest = {
    try { if(sessionCreated) session.close() }
    finally { testDB.cleanUp() }
  }
}

abstract class DBTestObject(dbs: TestDB.TestDBSpec*) {
  val testClassName = {
    val s = getClass.getName
    s.substring(0, s.length-1)
  }
  def main(args: Array[String]) {
    val res = JUnitCore.runClasses(Class.forName(testClassName))
    for(f <- res.getFailures) {
      f.getException.printStackTrace()
    }
  }
  @Parameters def parameters = JavaConversions.asList(dbs.map(n => Array(n(this))))
}

class TestDB(url: String, jdbcDriver: String, val dbType: String, val driver: ExtendedProfile) {
  override def toString = url
  def createDB() = Database.forURL(url, driver = jdbcDriver)
  def cleanUp() {}
  def deleteDBFiles(prefix: String) {
    val dir = new File(TestDB.testDBDir)
    if(!dir.isDirectory) throw new IOException("Directory "+TestDB.testDBDir+" not found")
    for(f <- dir.listFiles if f.getName startsWith prefix) {
      val p = TestDB.testDBDir+"/"+f.getName
      if(f.delete) println("[Deleted database file "+p+"]")
      else throw new IOException("Couldn't delete database file "+p)
    }
  }
}

object TestDB {
  type TestDBSpec = (DBTestObject => TestDB)
  private val testDBDir = "test-dbs"
  def H2Mem(to: DBTestObject) = new TestDB("jdbc:h2:mem:test1", "org.h2.Driver", "h2", H2Driver)
  def H2Disk(to: DBTestObject) = {
    val prefix = "h2-"+to.testClassName
    new TestDB("jdbc:h2:./"+TestDB.testDBDir+"/"+prefix, "org.h2.Driver", "h2", H2Driver) {
      override def cleanUp() = deleteDBFiles(prefix)
    }
  }
  def SQLiteMem(to: DBTestObject) = new TestDB("jdbc:sqlite::memory:", "org.sqlite.JDBC", "sqlite", null)
  def SQLiteDisk(to: DBTestObject) = {
    val prefix = "sqlite-"+to.testClassName
    new TestDB("jdbc:sqlite:./"+TestDB.testDBDir+"/"+prefix+".db", "org.sqlite.JDBC", "sqlite", null) {
      override def cleanUp() = deleteDBFiles(prefix)
    }
  }
}

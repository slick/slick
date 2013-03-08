package scala.slick.test.jdbc

import java.io.PrintWriter
import org.junit.Test
import org.junit.Assert._
import scala.slick.driver.{ H2Driver, PostgresDriver }
import scala.slick.jdbc.meta._
import scala.slick.jdbc.{ StaticQuery => Q }
import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.TestDB
import scala.slick.typeProviders.TypeProvider

object TypeProviderH2Mem extends (String => TestDB) {
  def apply(str: String) = new TestDB("h2mem", H2Driver) {
    val url = "jdbc:h2:mem"
    val jdbcDriver = "org.h2.Driver"
    override def isPersistent = false
    override lazy val capabilities = driver.capabilities + TestDB.plainSql + TestDB.plainSqlWide
  }
}

object TypeProviderTest extends DBTestObject(TypeProviderH2Mem)

class TypeProviderTest(val tdb: TestDB) extends DBTest {
  @Test def simpleTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-test")
    import Db1.driver.simple._
    import Database.threadLocalSession
    Db1.database.withSession {
      val q1 = Query(Db1.Suppliers.length)
      assertEquals("Size of Suppliers before change",
        q1.first, 3)
      Db1.Suppliers.insert(Db1.Supplier(1, "1", "2", "3", "4", "5"))
      val q2 = Query(Db1.Suppliers.length)
      assertEquals("Size of Suppliers after change",
        q2.first, 4)
      val q3 = Query(Db1.Coffees.length)
      assertEquals("Size of Coffees",
        q3.first, 1)
      val r = Query(Db1.Coffees).list.head
      val c: Db1.Coffee = r
      assertEquals("First element of Coffees", c, Db1.Coffee("coffee", 1, 2.3, 4, 5))
    }
  }

  @Test def fkTest1() {
    object Db1 extends TypeProvider.Db("", "type-providers-test-fk-1")
    import Db1.driver.simple._
    import Database.threadLocalSession
    Db1.database.withSession {
      val tables = MTable.getTables(Some(""), Some(""), None, None).list
      val a = tables.find(_.name.name equals "a").get
      val b = tables.find(_.name.name equals "b").get
      assertEquals("# of FKs of 'a' should be 1",
        a.getImportedKeys.list.length, 1)
      assertEquals("# of FKs of 'b' should be 0",
        b.getImportedKeys.list.length, 0)
    }
  }

  @Test def fkTest2() {
    object Db1 extends TypeProvider.Db("", "type-providers-test-fk-2")
    import Db1.driver.simple._
    import Database.threadLocalSession
    Db1.database.withSession {
      val tables = MTable.getTables(Some(""), Some(""), None, None).list
      val a = tables.find(_.name.name equals "a").get
      val b = tables.find(_.name.name equals "b").get
      assertEquals("# of FKs of 'a' should be 2",
        a.getImportedKeys.list.length, 2)
      assertEquals("# of FKs of 'b' should be 0",
        b.getImportedKeys.list.length, 0)
      val aFks = a.getImportedKeys.list
      assertTrue("FKs should match", aFks.map(fk => (fk.pkColumn, fk.fkColumn)) equals List(("f1", "k1"), ("f2", "k2")))
      assertTrue("FKs should be from 'a' to 'b'", aFks.map(fk => (fk.pkTable.name, fk.fkTable.name)) equals List(("b", "a"), ("b", "a")))
    }
  }
}

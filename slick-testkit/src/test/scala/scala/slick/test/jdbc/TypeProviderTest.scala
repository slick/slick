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
import scala.slick.typeproviders.TypeProvider

class TypeProviderTest {
  @Test def h2memTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-h2mem")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val q1 = Query(Suppliers.length)
      assertEquals("Size of Suppliers before change",
        q1.first, 3)
      Suppliers.insert(Supplier(1, "1", "2", "3", "4", "5"))
      val q2 = Query(Suppliers.length)
      assertEquals("Size of Suppliers after change",
        q2.first, 4)
      val q3 = Query(Coffees.length)
      assertEquals("Size of Coffees",
        q3.first, 1)
      val r = Query(Coffees).list.head
      val c: Coffee = r
      assertEquals("First element of Coffees", c, Coffee("coffee", 1, 2.3, 4, 5))
    }
  }

  @Test def hsqlSimpleTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-hsql")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val q1 = Query(Suppliers.length)
      assertEquals("Size of Suppliers before change",
        q1.first, 3)
    }
  }

  @Test def hsqlComplexTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-hsql")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val supps = Query(Suppliers).list
      val correctSupps = List(Supplier(49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
        Supplier(101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
        Supplier(150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966"))
      assertTrue("Checking each element of suppliers", supps.sameElements(correctSupps))
    }
  }

  @Test def sqliteSimpleTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-sqlite")
    import Db1.driver.simple._
    import Database.threadLocalSession
    Db1.database.withSession {
      val q1 = Query(Db1.Suppliers.length)
      assertEquals("Size of Suppliers before change",
        q1.first, 3)
    }
  }

  @Test def sqliteComplexTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-sqlite")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val supps = Query(Suppliers).list
      val correctSupps = List(Supplier(101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
        Supplier(49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
        Supplier(150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966"))
      assertTrue("Checking each element of suppliers", supps.sameElements(correctSupps))
    }
  }

  @Test def fkTest1() {
    object Db1 extends TypeProvider.Db("", "type-providers-h2mem-fk-1")
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
    object Db1 extends TypeProvider.Db("", "type-providers-h2mem-fk-2")
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

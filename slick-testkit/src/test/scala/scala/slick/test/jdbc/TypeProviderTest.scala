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
import scala.slick.schema.Naming
import scala.slick.ast.Select
import scala.slick.ast.Node

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

  @Test def fkSimpleTest() {
    def convertColumnsToString(columns: List[Node]): List[String] =
      columns.map(convertColumnNodeToString)
    object Db1 extends TypeProvider.Db("", "type-providers-h2mem-fk-1")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    Db1.database.withSession {
      val tables = MTable.getTables(Some(""), Some(""), None, None).list
      val a = tables.find(_.name.name equals "a").get
      val b = tables.find(_.name.name equals "b").get
      assertEquals("# of FKs of 'a' should be 1",
        1, A.foreignKeys.size)
      assertEquals("# of FKs of 'b' should be 0",
        0, B.foreignKeys.size)
      val aFk = A.foreignKeys.head
      val srcColumns = convertColumnsToString(aFk.linearizedSourceColumns.toList)
      val trgColumns = convertColumnsToString(aFk.linearizedTargetColumns.toList)
      assertEquals("FKs should have the same source column", srcColumns, List("k1"))
      assertEquals("FKs should have the same target column", trgColumns, List("f1"))
      assertTrue("FKs should be from 'a' to 'b'", aFk.sourceTable.isInstanceOf[A.type] && aFk.targetTable.isInstanceOf[B.type])
    }
  }

  @Test def fkCompoundTest() {
    def convertColumnsToString(columns: List[Node]): List[String] =
      columns.map(convertColumnNodeToString)
    object Db1 extends TypeProvider.Db("", "type-providers-h2mem-fk-2")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    Db1.database.withSession {
      assertEquals("# of FKs of 'a' should be 1",
        1, A.foreignKeys.size)
      assertEquals("# of FKs of 'b' should be 0",
        0, B.foreignKeys.size)
      val aFk = A.foreignKeys.head
      val srcColumns = convertColumnsToString(aFk.linearizedSourceColumns.toList)
      val trgColumns = convertColumnsToString(aFk.linearizedTargetColumns.toList)
      assertEquals("FKs should have the same source column", srcColumns, List("k1", "k2"))
      assertEquals("FKs should have the same target column", trgColumns, List("f1", "f2"))
      assertTrue("FKs should be from 'a' to 'b'", aFk.sourceTable.isInstanceOf[A.type] && aFk.targetTable.isInstanceOf[B.type])
    }
  }

  @Test def indexTest() {
    object Db1 extends TypeProvider.Db("", "type-providers-h2mem-fk-2")
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    Db1.database.withSession {
      assertEquals("# of unique indices of 'a' should be 0",
        0, A.indexes.size)
      assertEquals("# of unique indices of 'b' should be 1",
        1, B.indexes.size)
      val bIdx = B.indexes.head
      val bIdxFieldsName = bIdx.on map (convertColumnNodeToString)
      val columnNames = List("f1", "f2")
      assertTrue("Indices should refer to correct field", bIdxFieldsName sameElements columnNames)
      val indexName = Naming.indexName(columnNames)
      assertTrue("Indices should have the correct name", bIdx.name equals indexName)
    }
  }

  def convertColumnNodeToString(node: Node): String =
    node.asInstanceOf[Select].field.name
}

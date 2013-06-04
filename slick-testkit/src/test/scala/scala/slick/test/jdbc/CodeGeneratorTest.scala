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
import scala.slick.ast.Select
import scala.slick.ast.Node
import scala.slick.schema.naming.NamingDefault
import scala.slick.test.cg._

class CodeGeneratorTest {
  @Test def h2memTest() {
    val Db1 = CG1
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val q1 = Query(Suppliers.length)
      assertEquals("Size of Suppliers before change", 3,
        q1.first)
      Suppliers.insert(Supplier(1, "1", "2", "3", "4", "5"))
      val q2 = Query(Suppliers.length)
      assertEquals("Size of Suppliers after change", 4,
        q2.first)
      val q3 = Query(Coffees.length)
      assertEquals("Size of Coffees", 1,
        q3.first)
      val r = Query(Coffees).list.head
      val c: Coffee = r
      assertEquals("First element of Coffees", Coffee("coffee", 1, 2.3, 4, 5), c)
    }
  }

  @Test def hsqlSimpleTest() {
    val Db1 = CG2
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val q1 = Query(Suppliers.length)
      assertEquals("Size of Suppliers before change", 3,
        q1.first)
    }
  }

  @Test def hsqlComplexTest() {
    val Db1 = CG2
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
    val Db1 = CG3
    import Db1.driver.simple._
    import Database.threadLocalSession
    Db1.database.withSession {
      val q1 = Query(Db1.Suppliers.length)
      assertEquals("Size of Suppliers before change", 3,
        q1.first)
    }
  }

  @Test def sqliteComplexTest() {
    val Db1 = CG3
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
    val Db1 = CG4
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
      assertEquals("FKs should have the same source column", List("k1"), srcColumns)
      assertEquals("FKs should have the same target column", List("f1"), trgColumns)
      assertTrue("FKs should be from 'a' to 'b'", aFk.sourceTable.isInstanceOf[A.type] && aFk.targetTable.isInstanceOf[B.type])
    }
  }

  @Test def fkCompoundTest() {
    def convertColumnsToString(columns: List[Node]): List[String] =
      columns.map(convertColumnNodeToString)
    val Db1 = CG5
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
      assertEquals("FKs should have the same source column", List("k1", "k2"), srcColumns)
      assertEquals("FKs should have the same target column", List("f1", "f2"), trgColumns)
      assertTrue("FKs should be from 'a' to 'b'", aFk.sourceTable.isInstanceOf[A.type] && aFk.targetTable.isInstanceOf[B.type])
    }
  }

  @Test def indexTest() {
    val Db1 = CG5
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
      val indexName = NamingDefault.indexName(columnNames)
      assertTrue("Indices should have the correct name", bIdx.name equals indexName)
    }
  }

  @Test def autoIncTest() {
    def optionsOfColumn(c: scala.slick.lifted.Column[_]) =
      c.nodeDelegate.asInstanceOf[Select].field.asInstanceOf[scala.slick.ast.FieldSymbol].options.toList

    val Db1 = CG6
    import Db1._
    val k1Options = optionsOfColumn(A.k1)
    val k2Options = optionsOfColumn(A.k2)
    val sOptions = optionsOfColumn(A.s)
    assertTrue("k1 should be AutoInc",
      k1Options.exists(option => (option equals A.O.AutoInc)))
    assertTrue("k2 should not be AutoInc",
      k2Options.forall(option => !(option equals A.O.AutoInc)))
    assertTrue("s should not be AutoInc",
      sOptions.forall(option => !(option equals A.O.AutoInc)))
  }

  @Test def customNamingTest() {
    val Db1 = CG7
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    database.withSession {
      val q1 = Query(Supps.length)
      assertEquals("Size of Suppliers before change", 3,
        q1.first)
      Supps.insert(Supplier(1, "1", "2", "3", "4", "5"))
      val q2 = Query(Supps.length)
      assertEquals("Size of Suppliers after change", 4,
        q2.first)
      val q3 = Query(Coffs.length)
      assertEquals("Size of Coffees", 1,
        q3.first)
      val r = Query(Coffs).list.head
      val c: Coff = r
      assertEquals("First element of Coffees", Coff("coffee", 1, 2.3, 4, 5), c)
    }
  }

  @Test def customTypingTest() {
    val Db1 = CG8
    import Db1.driver.simple._
    import Database.threadLocalSession
    import Db1._
    import scala.slick.config.CustomTyping
    //    import CustomTyping.boolTypeMapper
    database.withSession {
      val q1 = Query(SimpleAs.length)
      assertEquals("Size of SimpleA before change", 0,
        q1.first)
      SimpleAs.insert(SimpleA(CustomTyping.True, "1"))
      val q2 = Query(SimpleAs.length)
      assertEquals("Size of SimpleA after change", 1,
        q2.first)
      val r = Query(SimpleAs).list.head
      val s: SimpleA = r
      assertEquals("First element of SimpleAs", SimpleA(CustomTyping.True, "1"), s)
    }
  }

  def convertColumnNodeToString(node: Node): String =
    node.asInstanceOf[Select].field.name
}

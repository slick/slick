package scala.slick.test.ql

import org.junit.Test
import org.junit.Assert._
import scala.slick.ql._
import scala.slick.driver.AccessDriver
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object DataTypeTest extends DBTestObject(H2Mem, SQLiteMem, HsqldbMem, MySQL, DerbyMem, Postgres, MSAccess, SQLServer)

class DataTypeTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  @Test def testByteArray() {
    object T extends Table[(Int, Array[Byte])]("test") {
      def id = column[Int]("id")
      def data = column[Array[Byte]]("data")
      def * = id ~ data
    }

    db withSession {
      T.ddl.createStatements foreach println
      T.ddl.create;
      T insert (1, Array[Byte](1,2,3))
      T insert (2, Array[Byte](4,5))
      assertEquals(Set((1,"123"), (2,"45")), Query(T).list.map{ case (id, data) => (id, data.mkString) }.toSet)
    }
  }

  @Test def testNumeric() = db withSession {
    object T extends Table[(Int, Int, Long, Short, Byte)]("test") {
      def id = column[Int]("id")
      def intData = column[Int]("int_data")
      def longData = column[Long]("long_data")
      def shortData = column[Short]("short_data")
      def byteData = column[Byte]("byte_data")
      def * = id ~ intData ~ longData ~ shortData ~ byteData
    }

    T.ddl.createStatements foreach println
    T.ddl.create;

    def test(data: List[(Int, Int, Long, Short, Byte)]) {
      T.insertAll(data: _*)
      val q = T.sortBy(_.id)
      assertEquals(data, q.list)
      Query(T).delete
    }

    test(List(
      (2, -1,          -1L,           -1: Short,      -1: Byte),
      (3, 0,            0L,            0: Short,       0: Byte),
      (4, 1,            1L,            1: Short,       1: Byte)
    ))

    test(List(
      (1, Int.MinValue, 0L, Short.MinValue, Byte.MinValue),
      (5, Int.MaxValue, 0L, Short.MaxValue, Byte.MaxValue)
    ))

    if(tdb.driver != AccessDriver) { // No proper LONG type support in Access via JDBC
      test(List(
        (1, 0, Long.MinValue, 0, 0),
        (5, 0, Long.MaxValue, 0, 0)
      ))
    }
  }
}

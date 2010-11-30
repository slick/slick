package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object ByteArrayTest extends DBTestObject(H2Mem, SQLiteMem, HsqldbMem, MySQL, DerbyMem, Postgres)

class ByteArrayTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def testByteArray() {
    object T extends Table[(Int, Array[Byte])]("test") {
      def id = column[Int]("id")
      def data = column[Array[Byte]]("data")
      def * = id ~ data
    }

    db withSession {
      T.ddl.create;
      T insert (1, Array[Byte](1,2,3))
      T insert (2, Array[Byte](4,5))
      assertEquals(Set((1,"123"), (2,"45")), Query(T).list.map{ case (id, data) => (id, data.mkString) }.toSet)
    }
  }
}

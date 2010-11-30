package org.scalaquery.test

import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob
import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ql._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object BlobTest extends DBTestObject(H2Mem, /* SQLiteMem, Postgres, HsqldbMem, */ MySQL, DerbyMem)

class BlobTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def testBlob() {
    object T extends Table[(Int, Blob)]("test") {
      def id = column[Int]("id")
      def data = column[Blob]("data")
      def * = id ~ data
    }

    // A Blob result does not survive a commit on all DBMSs so we wrap everything in a transaction
    db withTransaction {
      T.ddl.create;
      T insert (1, new SerialBlob(Array[Byte](1,2,3)))
      T insert (2, new SerialBlob(Array[Byte](4,5)))

      assertEquals(Set((1,"123"), (2,"45")),
        Query(T).list.map{ case (id, data) => (id, data.getBytes(1, data.length.toInt).mkString) }.toSet)
    }
  }
}

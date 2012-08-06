package scala.slick.test.lifted

import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob
import org.junit.Test
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil._
import scala.slick.testutil.TestDB._

object BlobTest extends DBTestObject(H2Mem, /* SQLiteMem, Postgres, HsqldbMem, */ MySQL, DerbyMem, SQLServer)

class BlobTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

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

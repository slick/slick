package com.typesafe.slick.testkit.tests

import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}
import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob
import org.junit.Assert._
import scala.slick.lifted._
import scala.slick.session.Database.threadLocalSession
import scala.slick.testutil.TestDB
import com.typesafe.slick.testkit.util.TestkitTest

//object BlobTest extends TestkitTestObject(H2Mem, /* SQLiteMem, Postgres, HsqldbMem, */ MySQL, DerbyMem, SQLServer)

class BlobTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.Table
  import tdb.profile.Implicit._

  def testBlob = if(cap.blob) {
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

  def testMappedBlob = if(cap.blob) {

    case class Serialized[T](value: T)

    implicit def serializedTypeMapper[T] = MappedTypeMapper.base[Serialized[T], Blob]({ s =>
      val b = new ByteArrayOutputStream
      val out = new ObjectOutputStream(b)
      out.writeObject(s.value)
      out.flush
      new SerialBlob(b.toByteArray)
    }, { b =>
      val in = new ObjectInputStream(b.getBinaryStream)
      Serialized[T](in.readObject().asInstanceOf[T])
    })

    object T extends Table[(Int, Serialized[List[Int]])]("t") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Serialized[List[Int]]]("b")
      def * = id ~ b
    }

    db withSession {
      T.ddl.create
      T.b.insertAll(Serialized(List(1,2,3)), Serialized(List(4,5)))
      assertEquals(Set((1, Serialized(List(1,2,3))), (2, Serialized(List(4,5)))), Query(T).list.toSet)
    }
  }
}

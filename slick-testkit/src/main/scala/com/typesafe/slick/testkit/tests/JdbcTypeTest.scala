package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}
import java.sql.{Blob, Date, Time, Timestamp}
import java.util.UUID
import javax.sql.rowset.serial.SerialBlob
import org.junit.Assert._

/** Data type related tests which are specific to JdbcProfile */
class JdbcTypeTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testByteArray {
    class T(tag: Tag) extends Table[(Int, Array[Byte])](tag, "test_ba") {
      def id = column[Int]("id")
      def data = column[Array[Byte]]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    ts.ddl.createStatements foreach println
    ts.ddl.create;
    ts insert (1, Array[Byte](1,2,3))
    ts insert (2, Array[Byte](4,5))
    assertEquals(
      Set((1,"123"), (2,"45")),
      ts.list.map{ case (id, data) => (id, data.mkString) }.toSet
    )
  }

  def testByteArrayOption {
    class T(tag: Tag) extends Table[(Int, Option[Array[Byte]])](tag, "test_baopt") {
      def id = column[Int]("id")
      def data = column[Option[Array[Byte]]]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    ts.ddl.createStatements foreach println
    ts.ddl.create;
    ts insert (1, Some(Array[Byte](6,7)))
    ifCap(rcap.setByteArrayNull)(ts.insert(2, None))
    ifNotCap(rcap.setByteArrayNull)(ts.map(_.id).insert(2))
    assertEquals(
      Set((1,"67"), (2,"")),
      ts.list.map{ case (id, data) => (id, data.map(_.mkString).getOrElse("")) }.toSet
    )
  }

  def testBlob = ifCap(rcap.typeBlob) {
    class T(tag: Tag) extends Table[(Int, Blob)](tag, "test3") {
      def id = column[Int]("id")
      def data = column[Blob]("data")
      def * = (id, data)
    }
    val ts = TableQuery[T]

    sharedSession.withTransaction {
      ts.ddl.create;
      ts insert (1, new SerialBlob(Array[Byte](1,2,3)))
      ts insert (2, new SerialBlob(Array[Byte](4,5)))

      assertEquals(Set((1,"123"), (2,"45")),
        ts.mapResult{ case (id, data) => (id, data.getBytes(1, data.length.toInt).mkString) }.to[Set])
    }
  }

  def testMappedBlob = ifCap(rcap.typeBlob) {
    case class Serialized[T](value: T)

    implicit def serializedType[T] = MappedColumnType.base[Serialized[T], Blob]({ s =>
      val b = new ByteArrayOutputStream
      val out = new ObjectOutputStream(b)
      out.writeObject(s.value)
      out.flush
      new SerialBlob(b.toByteArray)
    }, { b =>
      val in = new ObjectInputStream(b.getBinaryStream)
      Serialized[T](in.readObject().asInstanceOf[T])
    })

    class T(tag: Tag) extends Table[(Int, Serialized[List[Int]])](tag, "t") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Serialized[List[Int]]]("b")
      def * = (id, b)
    }
    val ts = TableQuery[T]

    sharedSession.withTransaction {
      ts.ddl.create
      ts.map(_.b).insertAll(Serialized(List(1,2,3)), Serialized(List(4,5)))
      assertEquals(Set((1, Serialized(List(1,2,3))), (2, Serialized(List(4,5)))), ts.list.toSet)
    }
  }

  private def roundtrip[T : BaseColumnType](tn: String, v: T, literal: Boolean = true, bind: Boolean = true) {
    class T1(tag: Tag) extends Table[(Int, T)](tag, tn) {
      def id = column[Int]("id")
      def data = column[T]("data")
      def * = (id, data)
    }
    val t1 = TableQuery[T1]

    t1.ddl.create
    t1.insert((1, v))
    assertEquals(v, t1.map(_.data).first)
    if(literal) {
      assertEquals(Some(1), t1.filter(_.data === v).map(_.id).firstOption)
      assertEquals(None, t1.filter(_.data =!= v).map(_.id).firstOption)
    }
    if(bind) {
      assertEquals(Some(1), t1.filter(_.data === v.bind).map(_.id).firstOption)
      assertEquals(None, t1.filter(_.data =!= v.bind).map(_.id).firstOption)
    }
  }

  def testDate =
    roundtrip("date_t1", Date.valueOf("2012-12-24"))

  def testTime =
    roundtrip("time_t1", Time.valueOf("17:53:48"))

  def testTimestamp = {
    roundtrip[Timestamp]("timestamp_t1", Timestamp.valueOf("2012-12-24 17:53:48.0"))

    class T2(tag: Tag) extends Table[Option[Timestamp]](tag, "timestamp_t2") {
      def t = column[Option[Timestamp]]("t")
      def * = t
    }
    val t2 = TableQuery[T2]
    t2.ddl.create
    t2.insert(None)
    assertEquals(None, t2.first)
  }

  def testUUID =
    roundtrip[UUID]("uuid_t1", UUID.randomUUID(), literal = false)

  def testOverrideIdentityType {
    class T1(tag: Tag) extends Table[Int](tag, "t1") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc, O.DBType("_FOO_BAR_"))
      def * = id
    }
    val t1 = TableQuery[T1]
    assertTrue(t1.ddl.createStatements.mkString.contains("_FOO_BAR_"))
  }
}

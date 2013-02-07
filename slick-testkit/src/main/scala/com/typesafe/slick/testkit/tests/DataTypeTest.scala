package com.typesafe.slick.testkit.tests

import scala.slick.ast.NumericTypedType
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}
import java.io.{ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}
import java.sql.{Blob, Date, Time, Timestamp}
import java.util.UUID
import javax.sql.rowset.serial.SerialBlob
import org.junit.Assert._

class DataTypeTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testByteArray {
    object T extends Table[(Int, Array[Byte])]("test_ba") {
      def id = column[Int]("id")
      def data = column[Array[Byte]]("data")
      def * = id ~ data
    }

    T.ddl.createStatements foreach println
    T.ddl.create;
    T insert (1, Array[Byte](1,2,3))
    T insert (2, Array[Byte](4,5))
    assertEquals(
      Set((1,"123"), (2,"45")),
      Query(T).list.map{ case (id, data) => (id, data.mkString) }.toSet
    )
  }

  def testByteArrayOption {
    object T extends Table[(Int, Option[Array[Byte]])]("test_baopt") {
      def id = column[Int]("id")
      def data = column[Option[Array[Byte]]]("data")
      def * = id ~ data
    }

    T.ddl.createStatements foreach println
    T.ddl.create;
    T insert (1, Some(Array[Byte](6,7)))
    ifCap(rcap.setByteArrayNull)(T.insert(2, None))
    ifNotCap(rcap.setByteArrayNull)(T.id.insert(2))
    assertEquals(
      Set((1,"67"), (2,"")),
      Query(T).list.map{ case (id, data) => (id, data.map(_.mkString).getOrElse("")) }.toSet
    )
  }

  def testNumeric {
    def testStore[T](values: T*)(implicit tm: BaseColumnType[T] with NumericTypedType) {
      object Tbl extends Table[(Int, T)]("test_numeric") {
        def id = column[Int]("id")
        def data = column[T]("data")
        def * = id ~ data
      }
      Tbl.ddl.create;
      val data = values.zipWithIndex.map { case (d, i) => (i+1, d) }
      Tbl.insertAll(data: _*)
      val q = Tbl.sortBy(_.id)
      assertEquals(data, q.list)
      Tbl.ddl.drop
    }

    testStore[Int](-1, 0, 1, Int.MinValue, Int.MaxValue)
    ifCap(rcap.typeLong) { testStore[Long](-1L, 0L, 1L, Long.MinValue, Long.MaxValue) }
    testStore[Short](-1, 0, 1, Short.MinValue, Short.MaxValue)
    testStore[Byte](-1, 0, 1, Byte.MinValue, Byte.MaxValue)
    testStore[Double](-1.0, 0.0, 1.0)
    testStore[Float](-1.0f, 0.0f, 1.0f)
    ifCap(rcap.typeBigDecimal) {
      testStore[BigDecimal](BigDecimal("-1"), BigDecimal("0"), BigDecimal("1"),
        BigDecimal(Long.MinValue), BigDecimal(Long.MaxValue))
    }
  }

  def testBlob = ifCap(rcap.typeBlob) {
    object T extends Table[(Int, Blob)]("test3") {
      def id = column[Int]("id")
      def data = column[Blob]("data")
      def * = id ~ data
    }

    sharedSession.withTransaction {
      T.ddl.create;
      T insert (1, new SerialBlob(Array[Byte](1,2,3)))
      T insert (2, new SerialBlob(Array[Byte](4,5)))

      assertEquals(Set((1,"123"), (2,"45")),
        Query(T).mapResult{ case (id, data) => (id, data.getBytes(1, data.length.toInt).mkString) }.to[Set])
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

    object T extends Table[(Int, Serialized[List[Int]])]("t") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def b = column[Serialized[List[Int]]]("b")
      def * = id ~ b
    }

    sharedSession.withTransaction {
      T.ddl.create
      T.b.insertAll(Serialized(List(1,2,3)), Serialized(List(4,5)))
      assertEquals(Set((1, Serialized(List(1,2,3))), (2, Serialized(List(4,5)))), Query(T).list.toSet)
    }
  }

  private def roundtrip[T : BaseColumnType](tn: String, v: T, literal: Boolean = true, bind: Boolean = true) {
    object T1 extends Table[(Int, T)](tn) {
      def id = column[Int]("id")
      def data = column[T]("data")
      def * = id ~ data
    }

    T1.ddl.create
    T1.insert((1, v))
    assertEquals(v, T1.map(_.data).first)
    if(literal) {
      assertEquals(Some(1), T1.filter(_.data === v).map(_.id).firstOption)
      assertEquals(None, T1.filter(_.data =!= v).map(_.id).firstOption)
    }
    if(bind) {
      assertEquals(Some(1), T1.filter(_.data === v.bind).map(_.id).firstOption)
      assertEquals(None, T1.filter(_.data =!= v.bind).map(_.id).firstOption)
    }
  }

  def testDate =
    roundtrip("date_t1", Date.valueOf("2012-12-24"))

  def testTime =
    roundtrip("time_t1", Time.valueOf("17:53:48"))

  def testTimestamp =
    roundtrip[Timestamp]("timestamp_t1", Timestamp.valueOf("2012-12-24 17:53:48.0"))

  def testUUID =
    roundtrip[UUID]("uuid_t1", UUID.randomUUID(), literal = false)
}

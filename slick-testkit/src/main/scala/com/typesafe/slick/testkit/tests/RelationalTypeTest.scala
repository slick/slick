package com.typesafe.slick.testkit.tests

import scala.slick.ast.NumericTypedType
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}
import org.junit.Assert._

/** Data type related test cases for RelationalProfile */
class RelationalTypeTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testNumeric {
    def testStore[T](values: T*)(implicit tm: BaseColumnType[T] with NumericTypedType) {
      class Tbl(tag: Tag) extends Table[(Int, T)](tag, "test_numeric") {
        def id = column[Int]("id")
        def data = column[T]("data")
        def * = (id, data)
      }
      val tbl = TableQuery[Tbl]
      tbl.ddl.create;
      val data = values.zipWithIndex.map { case (d, i) => (i+1, d) }
      tbl ++= data
      val q = tbl.sortBy(_.id)
      assertEquals(data, q.run)
      tbl.ddl.drop
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

  private def roundtrip[T : BaseColumnType](tn: String, v: T) {
    class T1(tag: Tag) extends Table[(Int, T)](tag, tn) {
      def id = column[Int]("id")
      def data = column[T]("data")
      def * = (id, data)
    }
    val t1 = TableQuery[T1]

    t1.ddl.create
    t1 += (1, v)
    assertEquals(v, t1.map(_.data).run.head)
    assertEquals(Some(1), t1.filter(_.data === v).map(_.id).run.headOption)
    assertEquals(None, t1.filter(_.data =!= v).map(_.id).run.headOption)
    assertEquals(Some(1), t1.filter(_.data === v.bind).map(_.id).run.headOption)
    assertEquals(None, t1.filter(_.data =!= v.bind).map(_.id).run.headOption)
  }

  def testBoolean {
    roundtrip[Boolean]("boolean_true", true)
    roundtrip[Boolean]("boolean_false", false)
  }

  def testUnit {
    class T(tag: Tag) extends Table[Int](tag, "unit_t") {
      def id = column[Int]("id")
      def * = id
    }
    val ts = TableQuery[T]
    ts.ddl.create
    ts += 42
    assertEquals(Seq(()), ts.map(_ => ()).run)
    assertEquals(Seq(((), 42)), ts.map(a => ((), a)).run)
    assertEquals(Seq((42, ())), ts.map(a => (a, ())).run)
  }
}

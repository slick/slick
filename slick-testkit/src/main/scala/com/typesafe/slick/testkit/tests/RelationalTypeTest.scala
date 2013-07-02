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
}

package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{RelationalTestDB, TestkitTest}

class PrimaryKeyTest extends TestkitTest[RelationalTestDB] {
  import tdb.profile.simple._

  def test {

    class A(tag: Tag) extends Table[(Int, Int, String)](tag, "a") {
      def k1 = column[Int]("k1")
      def k2 = column[Int]("k2")
      def s = column[String]("s")
      def * = (k1, k2, s)
      def pk = primaryKey("pk_a", (k1, k2))
    }
    val as = TableQuery[A]

    as.baseTableRow.primaryKeys.foreach(println)
    assertEquals(Set("pk_a"), as.baseTableRow.primaryKeys.map(_.name).toSet)

    as.ddl.create

    as ++= Seq(
      (1, 1, "a11"),
      (1, 2, "a12"),
      (2, 1, "a21"),
      (2, 2, "a22")
    )

    assertFail { as += (1, 1, "a11-conflict") }
  }
}

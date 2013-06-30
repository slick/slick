package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class JdbcMiscTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testNullability {
    class T1(tag: Tag) extends Table[String](tag, "t1") {
      def a = column[String]("a")
      def * = a
    }
    val t1 = TableQuery[T1]

    class T2(tag: Tag) extends Table[String](tag, "t2") {
      def a = column[String]("a", O.Nullable)
      def * = a
    }
    val t2 = TableQuery[T2]

    class T3(tag: Tag) extends Table[Option[String]](tag, "t3") {
      def a = column[Option[String]]("a")
      def * = a
    }
    val t3 = TableQuery[T3]

    class T4(tag: Tag) extends Table[Option[String]](tag, "t4") {
      def a = column[Option[String]]("a", O.NotNull)
      def * = a
    }
    val t4 = TableQuery[T4]

    (t1.ddl ++ t2.ddl ++ t3.ddl ++ t4.ddl).create

    t1.insert("a")
    t2.insert("a")
    t3.insert(Some("a"))
    t4.insert(Some("a"))

    t2.insert(null.asInstanceOf[String])
    t3.insert(None)

    assertFail { t1.insert(null.asInstanceOf[String]) }
    assertFail { t4.insert(None) }
  }
}

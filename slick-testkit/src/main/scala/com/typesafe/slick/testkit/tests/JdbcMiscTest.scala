package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class JdbcMiscTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testNullability {
    object T1 extends Table[String]("t1") {
      def a = column[String]("a")
      def * = a
    }

    object T2 extends Table[String]("t2") {
      def a = column[String]("a", O.Nullable)
      def * = a
    }

    object T3 extends Table[Option[String]]("t3") {
      def a = column[Option[String]]("a")
      def * = a
    }

    object T4 extends Table[Option[String]]("t4") {
      def a = column[Option[String]]("a", O.NotNull)
      def * = a
    }

    (T1.ddl ++ T2.ddl ++ T3.ddl ++ T4.ddl).create

    T1.insert("a")
    T2.insert("a")
    T3.insert(Some("a"))
    T4.insert(Some("a"))

    T2.insert(null.asInstanceOf[String])
    T3.insert(None)

    assertFail { T1.insert(null.asInstanceOf[String]) }
    assertFail { T4.insert(None) }
  }
}

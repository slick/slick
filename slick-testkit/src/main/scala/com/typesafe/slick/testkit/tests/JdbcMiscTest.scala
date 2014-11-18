package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class JdbcMiscTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testNullability = {
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

    seq(
      (t1.schema ++ t2.schema ++ t3.schema ++ t4.schema).create,
      t1 += "a",
      t2 += "a",
      t3 += Some("a"),
      t4 += Some("a"),
      t2 += null.asInstanceOf[String],
      t3 += None,
      (t1 += null.asInstanceOf[String]).failed,
      (t4 += None).failed
    )
  }

  def testColumnOptions = {
    class Foo(tag: Tag) extends Table[String](tag, "posts") {
      def bar = column[String]("s", O.Length(20,varying=true), O DBType "VARCHAR(20)" )        
      def * = bar
    }
    Action.successful(()).flatMap { _ => TableQuery[Foo].schema.create }.failed.map(_.shouldBeA[SlickException])
  }
}

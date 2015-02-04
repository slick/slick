package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

import scala.slick.jdbc.{ResultSetHoldability, ResultSetConcurrency, ResultSetType, JdbcBackend}

class JdbcMiscTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  @deprecated("Testing deprecated O.Nullable / O.NotNull column options", "3.0")
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
      def bar = column[String]("s", O.Length(20,varying=true), O SqlType "VARCHAR(20)" )
      def * = bar
    }
    Action.successful(()).flatMap { _ => TableQuery[Foo].schema.create }.failed.map(_.shouldBeA[SlickException])
  }

  def testSimpleDBIO = {
    val getAutoCommit = SimpleDBIO[Boolean](_.connection.getAutoCommit)
    getAutoCommit.map(_ shouldBe true)
  }

  def testStatementParameters = {
    def check(sp: JdbcBackend.StatementParameters) =
      GetStatementParameters.map { csp => csp shouldBe sp }

    Action.seq(
      check(JdbcBackend.StatementParameters(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0)),
      Action.seq(
        check(JdbcBackend.StatementParameters(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0)),
        check(JdbcBackend.StatementParameters(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit, null, 100)).
          withStatementParameters(rsHoldability = ResultSetHoldability.HoldCursorsOverCommit, fetchSize = 100),
        check(JdbcBackend.StatementParameters(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0))
      ).withStatementParameters(rsType = ResultSetType.ScrollInsensitive),
      check(JdbcBackend.StatementParameters(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto, null, 0))
    )
  }

  def testOverrideStatements = {
    class T(tag: Tag) extends Table[Int](tag, u"t") {
      def id = column[Int]("a")
      def * = id
    }
    val t = TableQuery[T]

    val a1 = t.filter(_.id === 1)
    val a2 = t.filter(_.id === 2)

    seq(
      t.schema.create,
      t ++= Seq(1, 2, 3),
      a1.result.map(_ shouldBe Seq(1)),
      a1.result.overrideStatements(a2.result.statements).map(_ shouldBe Seq(2)),
      a1.result.head.map(_ shouldBe 1),
      a1.result.head.overrideStatements(a2.result.head.statements).map(_ shouldBe 2)
    )
  }
}

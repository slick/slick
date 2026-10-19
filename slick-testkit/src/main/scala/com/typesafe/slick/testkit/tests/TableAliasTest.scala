package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import slick.jdbc.MySQLProfile

import java.sql.SQLException

class TableAliasTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  /* MySQL rejects a subquery that reads the target table of a DELETE or UPDATE (error 1093),
   * with or without an alias. */
  private def unlessMySQL[R](f: => DBIO[R]): DBIO[Unit] =
    if (tdb.profile.isInstanceOf[MySQLProfile]) DBIO.successful(()) else f.map(_ => ())

  class Messages(tag: Tag) extends Table[(Int, String, Boolean)](tag, "table_alias_messages") {
    def id = column[Int]("id", O.PrimaryKey)
    def content = column[String]("content")
    def flag = column[Boolean]("flag")
    def * = (id, content, flag)
  }
  lazy val messages = TableQuery[Messages]

  private val messageRows = Seq((1, "Hello", false), (2, "Hello", false), (3, "Yo!", false))

  /** A subquery in a DELETE that reads the target table must not bind its columns to the outer
    * row (https://github.com/slick/slick/issues/1374). */
  def testDeleteWithSelfReferencingSubquery = unlessMySQL {
    val duplicates = messages.filter(m => messages.filter(_.content === m.content).length > 1)
    seq(
      messages.schema.create,
      messages ++= messageRows,
      duplicates.delete.map(_ shouldBe 2),
      messages.map(_.content).result.map(_ shouldBe Seq("Yo!"))
    ).andFinally(messages.schema.drop)
  }

  /** The same for an UPDATE filtered with `exists` (https://github.com/slick/slick/issues/1929). */
  def testUpdateWithSelfReferencingSubquery = unlessMySQL {
    val duplicates = messages.filter { m =>
      messages.filter(o => o.content === m.content && o.id =!= m.id).exists
    }
    seq(
      messages.schema.create,
      messages ++= messageRows,
      duplicates.map(_.flag).update(true).map(_ shouldBe 2),
      messages.sortBy(_.id).map(_.flag).result.map(_ shouldBe Seq(true, true, false))
    ).andFinally(messages.schema.drop)
  }

  class Values(tag: Tag) extends Table[(String, Int)](tag, "table_alias_values") {
    def g = column[String]("g")
    def v = column[Int]("v")
    def * = (g, v)
  }
  lazy val values = TableQuery[Values]

  /** A DELETE with a correlated `max` subquery on the same table keeps the maximum of each group
    * (https://github.com/slick/slick/issues/1635). */
  def testDeleteWithCorrelatedMax = unlessMySQL {
    val notGroupMax = values.filter(o => o.v < values.filter(_.g === o.g).map(_.v).max)
    seq(
      values.schema.create,
      values ++= Seq(("a", 1), ("a", 2), ("b", 3), ("b", 4)),
      notGroupMax.delete.map(_ shouldBe 2),
      values.sortBy(_.v).result.map(_ shouldBe Seq(("a", 2), ("b", 4)))
    ).andFinally(values.schema.drop)
  }

  class Users(tag: Tag) extends Table[(Int, String)](tag, "table_alias_users") {
    def id = column[Int]("id", O.PrimaryKey)
    def first = column[String]("first")
    def * = (id, first)
  }
  lazy val users = TableQuery[Users]

  /** The same table, mapped with a column that does not exist in the database. */
  class MismatchedUsers(tag: Tag) extends Table[(Int, String)](tag, "table_alias_users") {
    def id = column[Int]("id", O.PrimaryKey)
    def firstname = column[String]("firstname")
    def * = (id, firstname)
  }
  lazy val mismatchedUsers = TableQuery[MismatchedUsers]

  /** Selecting a column that does not exist must fail instead of returning the column name as a
    * string literal, as SQLite does for unqualified double-quoted identifiers
    * (https://github.com/slick/slick/issues/1402). */
  def testUnknownColumnFails = seq(
    users.schema.create,
    users += ((1, "Homer")),
    mismatchedUsers.result.failed.map(_ should (_.isInstanceOf[SQLException])),
    mismatchedUsers.filter(_.firstname === "Homer").map(_.id).result.failed.map(_ should (_.isInstanceOf[SQLException])),
    mismatchedUsers.map(_.firstname).result.failed.map(_ should (_.isInstanceOf[SQLException]))
  ).andFinally(users.schema.drop)
}

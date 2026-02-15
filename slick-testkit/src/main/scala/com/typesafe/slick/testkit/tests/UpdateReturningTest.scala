package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class UpdateReturningTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testUpdateReturning = ifCapF(jcap.returnInsertKey) {
    class Users(tag: Tag) extends Table[(Int, String, String)](tag, "update_returning_users") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def firstName = column[String]("first_name")
      def lastName = column[String]("last_name")
      def * = (id, firstName, lastName)
    }

    val users = TableQuery[Users]

    db.run(DBIO.seq(
      users.schema.create,
      users += (0, "John", "Doe"),
      users += (0, "Jane", "Smith"),
      
      // Test basic UPDATE RETURNING
      users.filter(_.id === 1).map(u => (u.firstName, u.lastName))
        .updateReturning(users.map(_.id))
        .update(("Johnny", "Doe")).map { result =>
        result shouldBe Some(1)
      },
      
      // Test UPDATE RETURNING with into
      users.filter(_.id === 2).map(u => (u.firstName, u.lastName))
        .updateReturning(users.map(_.id))
        .into((values, id) => (id, values._1, values._2))
        .update(("Janet", "Smith")).map { result =>
        result shouldBe Some((2, "Janet", "Smith"))
      },
      
      // Verify the updates worked
      users.sortBy(_.id).result.map { rows =>
        rows shouldBe Seq((1, "Johnny", "Doe"), (2, "Janet", "Smith"))
      }
    ))
  }
}
package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import slick.jdbc.MySQLProfile

/**
  * For the unqualified variant, see com.typesafe.slick.testkit.tests.ForeignKeyTest
  */
class MySQLQualifiedExtraTests extends AsyncTest[JdbcTestDB] {
  lazy val mysqlProfile = tdb.profile.asInstanceOf[MySQLProfile]
  import mysqlProfile.api._

  def testForeignKeyDrop = {

    class A(tag: Tag)
      extends Table[(Int, String)](tag, Some("slick_test"), "A") {
      def * = (id, script)

      val id: Rep[Int] = column[Int]("Id", O.AutoInc, O.PrimaryKey)

      val script: Rep[String] =
        column[String]("Script", O.Length(500, varying = true))
    }

    val as = TableQuery[A]

    class B(tag: Tag)
      extends Table[(Int, Int)](tag, Some("slick_test"), "B") {
      def * = (id, aid)

      val id: Rep[Int] = column[Int]("Id", O.AutoInc, O.PrimaryKey)

      val aid: Rep[Int] = column[Int]("AId")

      lazy val aFK = foreignKey("fkA_B", aid, as)(
        r => r.id,
        onUpdate = ForeignKeyAction.NoAction,
        onDelete = ForeignKeyAction.NoAction)
    }


    for {
      _ <- tdb.assertNotTablesExist("A", "B")
      _ <- (as.schema ++ as.schema).create
      _ <- tdb.assertTablesExist("A", "B")
      _ <- (as.schema ++ as.schema).drop
      _ <- tdb.assertNotTablesExist("A", "B")
    } yield ()
  }
}

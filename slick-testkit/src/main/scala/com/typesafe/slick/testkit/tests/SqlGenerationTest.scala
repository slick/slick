package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class SqlGenerationTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testForShareSqlGeneration: DBIO[Unit] = {
    class TestTable(tag: Tag) extends Table[(Int, String)](tag, "test_table") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val testTable = TableQuery[TestTable]

    val forShareQuery = testTable.forShare
    val forUpdateQuery = testTable.forUpdate
    
    // Test that both methods exist and compile
    val shareResult = forShareQuery.result
    val updateResult = forUpdateQuery.result
    
    // Simple test to verify they can be executed (syntax validation)
    ifCap(jcap.forUpdate) {
      for {
        _ <- testTable.schema.create
        _ <- testTable += (1, "test")
        _ <- shareResult.headOption.map(_ shouldBe Some((1, "test")))
        _ <- updateResult.headOption.map(_ shouldBe Some((1, "test")))
        _ <- testTable.schema.drop
      } yield ()
    }
  }
}
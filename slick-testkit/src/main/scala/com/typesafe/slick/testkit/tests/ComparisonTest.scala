package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class ComparisonTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testForUpdateAndForShareComparison: DBIO[Unit] = {
    class TestTable(tag: Tag) extends Table[(Int, String)](tag, "comparison_test") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val testTable = TableQuery[TestTable]

    val normalQuery = testTable
    val forShareQuery = testTable.forShare
    val forUpdateQuery = testTable.forUpdate
    
    // Test that all methods exist and compile
    val normalResult = normalQuery.result
    val shareResult = forShareQuery.result
    val updateResult = forUpdateQuery.result
    
    // Simple test to verify they can be executed (basic syntax validation)
    ifCap(jcap.forUpdate) {
      for {
        _ <- testTable.schema.create
        _ <- testTable += (1, "test")
        normalRow <- normalResult.headOption
        shareRow <- shareResult.headOption
        updateRow <- updateResult.headOption
        _ = normalRow shouldBe Some((1, "test"))
        _ = shareRow shouldBe Some((1, "test"))
        _ = updateRow shouldBe Some((1, "test"))
        _ <- testTable.schema.drop
      } yield ()
    }
  }
}
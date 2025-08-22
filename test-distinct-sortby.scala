// Test case to reproduce the distinctOn with outer sortBy issue

import slick.jdbc.PostgreSQLProfile.api._
import slick.jdbc.PostgreSQLProfile

object TestDistinctSortBy extends App {
  
  // Define tables similar to the issue description
  class Lists(tag: Tag) extends Table[(Int, String)](tag, "lists") {
    def id = column[Int]("id", O.PrimaryKey)
    def name = column[String]("name")
    def * = (id, name)
  }
  val lists = TableQuery[Lists]
  
  class ListItems(tag: Tag) extends Table[(Int, Option[Int], java.sql.Timestamp)](tag, "list_items") {
    def id = column[Int]("id", O.PrimaryKey)
    def listId = column[Option[Int]]("list_id")
    def createdAt = column[java.sql.Timestamp]("created_at")
    def * = (id, listId, createdAt)
  }
  val listItems = TableQuery[ListItems]
  
  // This is the problematic query from the issue
  val problemQuery = lists
    .joinLeft(listItems).on((list, item) => list.id === item.listId)
    .sortBy((list, item) => (list.id, item.createdAt.desc))
    .distinctOn((list, _) => list.id)
    // .subquery  // This should not be necessary according to documentation
    .sortBy((_, item) => item.createdAt.desc)
  
  println("Query compiled successfully")
  println(problemQuery.result.statements.head)
}
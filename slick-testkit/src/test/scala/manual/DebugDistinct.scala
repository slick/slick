package manual

import slick.jdbc.PostgresProfile.api._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object DebugDistinct extends App {
  
  class Lists(tag: Tag) extends Table[(Int, String)](tag, "lists_debug") {
    def id = column[Int]("id", O.PrimaryKey)
    def name = column[String]("name")
    def * = (id, name)
  }

  class ListItems(tag: Tag) extends Table[(Int, Option[Int], Int)](tag, "list_items_debug") {
    def id = column[Int]("id", O.PrimaryKey)
    def listId = column[Option[Int]]("list_id")
    def createdAt = column[Int]("created_at")
    def * = (id, listId, createdAt)
  }

  val lists = TableQuery[Lists]
  val listItems = TableQuery[ListItems]

  // This is the problematic query
  val problemQuery = lists
    .joinLeft(listItems).on((list, item) => list.id === item.listId)
    .sortBy { case (list, item) => (list.id, item.map(_.createdAt).desc) }
    .distinctOn { case (list, _) => list.id }
    .sortBy { case (_, item) => item.map(_.createdAt).desc }

  println("Compiled SQL:")
  println(problemQuery.result.statements.mkString("\n"))
  
  println("Trying to execute query (will fail since no DB is setup, but shows SQL compilation works)")
}
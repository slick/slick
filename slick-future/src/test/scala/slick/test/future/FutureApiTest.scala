package slick.test.future

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

import org.junit.Assert.assertEquals
import org.junit.Test

import slick.future.Database
import slick.jdbc.{DatabaseConfig, H2Profile}

class FutureApiTest {
  implicit val ec: ExecutionContext = ExecutionContext.global

  import H2Profile.api.*

  class Items(tag: Tag) extends Table[Int](tag, "future_api_items") {
    def id = column[Int]("id")
    def * = id
  }

  private val items = TableQuery[Items]
  private val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:FutureApiTest", driver = "org.h2.Driver", keepAliveConnection = true)

  @Test
  def openAndRunWorks(): Unit = {
    val db = Await.result(Database.open(dc), Duration.Inf)
    try {
      val action = for {
        _ <- items.schema.createIfNotExists
        _ <- items ++= Seq(1, 2, 3)
        rows <- items.sortBy(_.id).result
        _ <- items.schema.dropIfExists
      } yield rows

      val rows = Await.result(db.run(action), Duration.Inf)
      assertEquals(List(1, 2, 3), rows.toList)
    } finally db.close()
  }

  @Test
  def useWorks(): Unit = {
    val action = for {
      _ <- items.schema.createIfNotExists
      _ <- items ++= Seq(10, 20)
      rows <- items.sortBy(_.id).result
      _ <- items.schema.dropIfExists
    } yield rows

    val rows = Await.result(Database.use(dc)(db => db.run(action)), Duration.Inf)
    assertEquals(List(10, 20), rows.toList)
  }

  @Test
  def streamForeachWorks(): Unit = {
    val db = Await.result(Database.open(dc), Duration.Inf)
    try {
      Await.result(db.run(items.schema.createIfNotExists >> (items ++= Seq(1, 2, 3))), Duration.Inf)
      val buf = collection.mutable.ListBuffer.empty[Int]
      Await.result(db.stream(items.sortBy(_.id).result).foreach(buf += _), Duration.Inf)
      assertEquals(List(1, 2, 3), buf.toList)
      Await.result(db.run(items.schema.dropIfExists), Duration.Inf)
    } finally db.close()
  }

  @Test
  def streamIsLazyAndIndependent(): Unit = {
    val db = Await.result(Database.open(dc), Duration.Inf)
    try {
      Await.result(db.run(items.schema.createIfNotExists >> (items ++= Seq(1, 2, 3))), Duration.Inf)
      val publisher = db.stream(items.sortBy(_.id).result)

      // Each foreach triggers an independent execution
      val buf1 = collection.mutable.ListBuffer.empty[Int]
      val buf2 = collection.mutable.ListBuffer.empty[Int]
      Await.result(publisher.foreach(buf1 += _), Duration.Inf)
      Await.result(publisher.foreach(buf2 += _), Duration.Inf)
      assertEquals(List(1, 2, 3), buf1.toList)
      assertEquals(List(1, 2, 3), buf2.toList)

      Await.result(db.run(items.schema.dropIfExists), Duration.Inf)
    } finally db.close()
  }

  @Test
  def mapResultWorks(): Unit = {
    val db = Await.result(Database.open(dc), Duration.Inf)
    try {
      Await.result(db.run(items.schema.createIfNotExists >> (items ++= Seq(1, 2, 3))), Duration.Inf)
      val buf = collection.mutable.ListBuffer.empty[String]
      Await.result(db.stream(items.sortBy(_.id).result).mapResult(_.toString).foreach(buf += _), Duration.Inf)
      assertEquals(List("1", "2", "3"), buf.toList)
      Await.result(db.run(items.schema.dropIfExists), Duration.Inf)
    } finally db.close()
  }

  @Test
  def chainedMapResultWorks(): Unit = {
    val db = Await.result(Database.open(dc), Duration.Inf)
    try {
      Await.result(db.run(items.schema.createIfNotExists >> (items ++= Seq(1, 2, 3))), Duration.Inf)
      val buf = collection.mutable.ListBuffer.empty[String]
      Await.result(
        db.stream(items.sortBy(_.id).result)
          .mapResult(_ * 10)
          .mapResult(_.toString)
          .foreach(buf += _),
        Duration.Inf
      )
      assertEquals(List("10", "20", "30"), buf.toList)
      Await.result(db.run(items.schema.dropIfExists), Duration.Inf)
    } finally db.close()
  }

  @Test
  def chainedMapResultIsLazy(): Unit = {
    val db = Await.result(Database.open(dc), Duration.Inf)
    try {
      Await.result(db.run(items.schema.createIfNotExists >> (items ++= Seq(1, 2, 3))), Duration.Inf)
      val publisher = db.stream(items.sortBy(_.id).result).mapResult(_ * 10).mapResult(_.toString)

      // Each invocation triggers an independent execution
      val buf1 = collection.mutable.ListBuffer.empty[String]
      val buf2 = collection.mutable.ListBuffer.empty[String]
      Await.result(publisher.foreach(buf1 += _), Duration.Inf)
      Await.result(publisher.foreach(buf2 += _), Duration.Inf)
      assertEquals(List("10", "20", "30"), buf1.toList)
      assertEquals(List("10", "20", "30"), buf2.toList)

      Await.result(db.run(items.schema.dropIfExists), Duration.Inf)
    } finally db.close()
  }
}

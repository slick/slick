package com.typesafe.slick.docs

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import slick.future.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*

object ConnectingFuture {
  implicit val ec: ExecutionContext = ExecutionContext.global

  // DB_CLOSE_DELAY=-1 keeps the in-memory database between the separately opened Database instances below
  private val url = "jdbc:h2:mem:connectingfuture;DB_CLOSE_DELAY=-1"

  def main(args: Array[String]): Unit =
    try runExamples()
    finally shutdown(url)

  private def runExamples(): Unit = {
    val dc = DatabaseConfig.forURL(H2Profile, url, driver = "org.h2.Driver")

    val coffees = TableQuery[Coffees]

    val expected = Seq("Colombian", "Espresso")
    Await.result(
      Database.use(dc) { db =>
        db.run(coffees.schema.create >> (coffees ++= expected.map(name => (name, 101, 7.99, 0, 0))))
      },
      10.seconds
    )

    //#futureManaged
    val managedResult: Future[Seq[String]] =
      Database.use(dc) { db =>
        db.run(coffees.map(_.name).result)
      }
    //#futureManaged

    //#futureUnmanaged
    val opened: Future[Database] = Database.open(dc)
    //#futureUnmanaged

    //#futureRun
    val runResult: Future[Seq[String]] =
      Database.use(dc) { db =>
        db.run(coffees.map(_.name).result)
      }
    //#futureRun

    //#futureStream
    val streamResult: Future[slick.future.DatabasePublisher[String]] =
      Database.use(dc) { db =>
        Future.successful(db.stream(coffees.map(_.name).result))
      }
    //#futureStream

    // Each example gets the rows back
    val openedDb = Await.result(opened, 10.seconds)
    try {
      assert(Await.result(openedDb.run(coffees.map(_.name).result), 10.seconds).sorted == expected)
    } finally {
      openedDb.close()
    }

    assert(Await.result(managedResult, 10.seconds).sorted == expected)
    assert(Await.result(runResult, 10.seconds).sorted == expected)

    val streamed = ListBuffer.empty[String]
    Await.result(Await.result(streamResult, 10.seconds).foreach(streamed += _), 10.seconds)
    assert(streamed.sorted == expected, s"Expected the stream to emit $expected, got $streamed")
  }

  // DB_CLOSE_DELAY=-1 keeps the in-memory database after its connections close; SHUTDOWN removes it, so that the
  // next run in the same JVM starts from an empty database
  private def shutdown(url: String): Unit = {
    val connection = java.sql.DriverManager.getConnection(url)
    try connection.createStatement().execute("SHUTDOWN")
    finally connection.close()
  }

  final class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = (name, supID, price, sales, total)
  }
}

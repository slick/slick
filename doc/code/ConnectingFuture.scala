package com.typesafe.slick.docs

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import slick.future.Database
import slick.jdbc.H2Profile.api.*

object ConnectingFuture {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val url = ExampleDatabases.h2Url("connectingfuture")

  def main(args: Array[String]): Unit =
    try runExamples()
    finally ExampleDatabases.shutdown(url)

  private def runExamples(): Unit = {
    val dc = ExampleDatabases.pooledH2(url)

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
    val streamed: Future[Unit] =
      Database.use(dc) { db =>
        db.stream(coffees.map(_.name).result).foreach(println)
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
    // Streaming must finish without the pool rejecting the connection; the other examples check the rows themselves
    Await.result(streamed, 10.seconds)
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

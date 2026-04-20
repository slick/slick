package com.typesafe.slick.docs

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

  def main(args: Array[String]): Unit = {
    val dc = DatabaseConfig.forProfileConfig(H2Profile, "mydb")

    val coffees = TableQuery[Coffees]

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

    val openedDb = Await.result(opened, 10.seconds)
    try {
      Await.result(openedDb.run(coffees.map(_.name).result), 10.seconds)
    } finally {
      openedDb.close()
    }

    Await.result(managedResult, 10.seconds)
    Await.result(runResult, 10.seconds)
    Await.result(streamResult, 10.seconds)
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

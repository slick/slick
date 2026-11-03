package com.typesafe.slick.docs

import slick.jdbc.H2Profile.api.*
import slick.zio.Database

import zio.Runtime
import zio.Scope
import zio.Task
import zio.Unsafe
import zio.ZIO

object ConnectingZio {
  private val url = ExampleDatabases.h2Url("connectingzio")
  private val dc = ExampleDatabases.pooledH2(url)

  private val coffees = TableQuery[Coffees]

  //#zioManaged
  val managedResult: ZIO[Scope, Throwable, Seq[String]] =
    Database.scoped(dc).flatMap { db =>
      db.run(coffees.map(_.name).result)
    }
  //#zioManaged

  //#zioUnmanaged
  val opened: Task[Database] = Database.make(dc)
  //#zioUnmanaged

  //#zioRun
  val runResult: ZIO[Scope, Throwable, Seq[String]] =
    Database.scoped(dc).flatMap { db =>
      db.run(coffees.map(_.name).result)
    }
  //#zioRun

  //#zioStream
  val streamResult: ZIO[Scope, Throwable, zio.Chunk[String]] =
    Database.scoped(dc).flatMap { db =>
      db.stream(coffees.map(_.name).result).runCollect
    }
  //#zioStream

  private val expected = Seq("Colombian", "Espresso")

  private def check(example: String)(names: Seq[String]): Task[Unit] =
    ZIO.attempt(assert(names.sorted == expected, s"$example: expected $expected, got $names"))

  // Fills COFFEES, then checks that each example gets the rows back
  private val program: Task[Unit] =
    ZIO.scoped(
      Database.scoped(dc).flatMap(_.run(coffees.schema.create >> (coffees ++= expected.map(name => (name, 101, 7.99, 0, 0)))))
    ) *>
      ZIO.scoped(managedResult.flatMap(check("zioManaged"))) *>
      ZIO.scoped(runResult.flatMap(check("zioRun"))) *>
      ZIO.scoped(streamResult.flatMap(names => check("zioStream")(names.toSeq))) *>
      opened.flatMap { db =>
        db.run(coffees.map(_.name).result).flatMap(check("zioUnmanaged")).ensuring(ZIO.attemptBlocking(db.close()).ignore)
      }

  def main(args: Array[String]): Unit =
    try Unsafe.unsafe { implicit unsafe => Runtime.default.unsafe.run(program).getOrThrowFiberFailure() }
    finally ExampleDatabases.shutdown(url)

  final class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = (name, supID, price, sales, total)
  }
}

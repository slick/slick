package com.typesafe.slick.docs

import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*
import slick.zio.Database

import zio.Scope
import zio.Task
import zio.ZIO
import zio.ZIOAppDefault

object ConnectingZio extends ZIOAppDefault {
  private val dc = DatabaseConfig.forProfileConfig(H2Profile, "mydb")

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

  override def run: Task[Unit] =
    ZIO.scoped {
      managedResult.unit *>
      runResult.unit *>
      streamResult.unit
    } *>
      opened.flatMap { db =>
        db.run(coffees.map(_.name).result).unit.ensuring(ZIO.attemptBlocking(db.close()).ignore)
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

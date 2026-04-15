package com.typesafe.slick.docs

//#imports
import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

import _root_.cats.effect.IO
import _root_.cats.effect.Resource
import _root_.cats.effect.unsafe.implicits.global

import slick.cats
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api._
//#imports

object Connection {
  def main(args: Array[String]): Unit = {
    class Coffees(tag: Tag) extends Table[(String, Blob)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def image = column[Blob]("IMAGE")
      def * = (name, image)
    }
    val coffees = TableQuery[Coffees]
    if (false){
      val dataSource = null.asInstanceOf[javax.sql.DataSource]
      val size = 42
      //#forDataSource
      val dc = DatabaseConfig.forDataSource(H2Profile, dataSource: javax.sql.DataSource, Some(size: Int))
      val db: Resource[IO, cats.Database] = cats.Database.resource(dc)
      //#forDataSource
    }
    if (false){
      val dataSource = null.asInstanceOf[slick.jdbc.DatabaseUrlDataSource]
      //#forDatabaseURL
      val dc = DatabaseConfig.forDataSource(H2Profile, dataSource: slick.jdbc.DatabaseUrlDataSource, None)
      val db = cats.Database.resource(dc)
      //#forDatabaseURL
    }
    if(false) {
      val jndiName = ""
      val size = 42
      //#forName
      val dc = DatabaseConfig.forName(H2Profile, jndiName: String, Some(size: Int))
      val db = cats.Database.resource(dc)
      //#forName
    }
    ;{
      //#forConfig
      val dc = DatabaseConfig.forProfileConfig(H2Profile, "mydb")
      val db = cats.Database.resource(dc)
      //#forConfig
      // use `db` as a Resource: db.use { ... }
    }
    ;{
      //#forURL
      val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
      val db = cats.Database.resource(dc)
      //#forURL
      // use `db` as a Resource: db.use { ... }
    }

    val urlDc = DatabaseConfig.forURL(
      H2Profile,
      "jdbc:h2:mem:test2;INIT=" + coffees.schema.createStatements.mkString("\\;"),
      driver = "org.h2.Driver"
    )
    val dbResource = cats.Database.resource(urlDc)
    dbResource.use { db =>
      val lines = new ArrayBuffer[Any]()
      def println(s: Any) = lines += s

      ;{
        //#materialize
        val q = for (c <- coffees) yield c.name
        val a = q.result
        val f: IO[Seq[String]] = db.run(a)

        f.map {
          case s => println(s"Result: $s")
        }
        //#materialize
      }.flatMap { _ =>
        ;{
          //#stream
          val q = for (c <- coffees) yield c.name
          val a = q.result
          val s: fs2.Stream[IO, String] = db.stream(a)

          // Use FS2 combinators to process the stream:
          s.evalMap(name => IO(println(s"Element: $name")))
            .compile
            .drain
          //#stream
        }.flatMap { _ =>
          ;{
            //#streamblob
            val q = for (c <- coffees) yield c.image
            val a = q.result
            val s: fs2.Stream[IO, Blob] = db.stream(a)
            val bytesStream: fs2.Stream[IO, Array[Byte]] =
              s.map(b => b.getBytes(0, b.length().toInt))
            //#streamblob
            bytesStream.compile.drain
          }.flatMap { _ =>
            ;{
              //#transaction
              val a = (for {
                ns <- coffees.filter(_.name.startsWith("ESPRESSO")).map(_.name).result
                _ <- DBIO.seq(ns.map(n => coffees.filter(_.name === n).delete): _*)
              } yield ()).transactionally

              val f: IO[Unit] = db.run(a)
              //#transaction
              f
            }.flatMap { _ =>
              ;{
                //#rollback
                val countAction = coffees.length.result

                val rollbackAction = (coffees ++= Seq(
                  ("Cold_Drip", new SerialBlob(Array[Byte](101))),
                  ("Dutch_Coffee", new SerialBlob(Array[Byte](49)))
                )).flatMap { _ =>
                  DBIO.failed(new Exception("Roll it back"))
                }.transactionally

                val errorHandleAction = rollbackAction.asTry.flatMap {
                  case Failure(e: Throwable) => DBIO.successful(e.getMessage)
                  case Success(_) => DBIO.successful("never reached")
                }

                // Here we show that that coffee count is the same before and after the attempted insert.
                // We also show that the result of the action is filled in with the exception's message.
                val f = db.run(countAction zip errorHandleAction zip countAction).map {
                  case ((initialCount, result), finalCount) =>
                    // init: 5, final: 5, result: Roll it back
                    println(s"init: ${initialCount}, final: ${finalCount}, result: ${result}")
                    result
                }

                //#rollback
                f.map { result =>
                  assert(result == "Roll it back")
                }
              }
            }
          }
        }
      }.map { _ =>
        lines.foreach(Predef.println _)
      }
    }.unsafeRunSync()

    //#simpleaction
    val getAutoCommit = SimpleDBIO[Boolean](_.connection.getAutoCommit)
    //#simpleaction
  }
}

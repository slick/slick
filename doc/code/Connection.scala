package com.typesafe.slick.docs

//#imports
import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

import cats.effect.IO
import cats.effect.unsafe.implicits.global

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
      val db = Database.forDataSource[IO](dataSource: javax.sql.DataSource,
        Some(size: Int))
      //#forDataSource
    }
    if (false){
      val dataSource = null.asInstanceOf[slick.jdbc.DatabaseUrlDataSource]
      //#forDatabaseURL
      val db = Database.forDataSource[IO](dataSource: slick.jdbc.DatabaseUrlDataSource,
        None)
      //#forDatabaseURL
    }
    if(false) {
      val jndiName = ""
      val size = 42
      //#forName
      val db = Database.forName[IO](jndiName: String, Some(size: Int))
      //#forName
    }
    ;{
      //#forConfig
      val db = Database.forConfig[IO]("mydb")
      //#forConfig
      // use `db` as a Resource: db.use { ... }
    }
    ;{
      //#forURL
      val db = Database.forURL[IO]("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1",
        driver="org.h2.Driver")
      //#forURL
      // use `db` as a Resource: db.use { ... }
    }

    val dbResource = Database.forURL[IO]("jdbc:h2:mem:test2;INIT="+coffees.schema.createStatements.mkString("\\;"), driver="org.h2.Driver")
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

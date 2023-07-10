package com.typesafe.slick.docs

//#imports
import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import slick.basic.DatabasePublisher
import slick.jdbc.H2Profile.api._
//#imports

object Connection extends App {
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
    val db = Database.forDataSource(dataSource: javax.sql.DataSource,
      Some(size: Int))
    //#forDataSource
  }
  if (false){
    val dataSource = null.asInstanceOf[slick.jdbc.DatabaseUrlDataSource]
    //#forDatabaseURL
    val db = Database.forDataSource(dataSource: slick.jdbc.DatabaseUrlDataSource,
      None)
    //#forDatabaseURL
  }
  if(false) {
    val jndiName = ""
    val size = 42
    //#forName
    val db = Database.forName(jndiName: String, Some(size: Int))
    //#forName
  }
  ;{
    //#forConfig
    val db = Database.forConfig("mydb")
    //#forConfig
    db.close
  }
  ;{
    //#forURL
    val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1",
      driver="org.h2.Driver")
    //#forURL
    db.close
  }
  ;{
    //#forURL2
    val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1",
      driver="org.h2.Driver",
      executor = AsyncExecutor("test1", numThreads=10, queueSize=1000))
    //#forURL2
    db.close
  }
  val db = Database.forURL("jdbc:h2:mem:test2;INIT="+coffees.schema.createStatements.mkString("\\;"), driver="org.h2.Driver")
  try {
    val lines = new ArrayBuffer[Any]()
    def println(s: Any) = lines += s
    ;{
      //#materialize
      val q = for (c <- coffees) yield c.name
      val a = q.result
      val f: Future[Seq[String]] = db.run(a)

      f.onComplete {
        case Success(s) => println(s"Result: $s")
        case Failure(t) => t.printStackTrace()
      }
      //#materialize
      Await.result(f, Duration.Inf)
    };{
      //#stream
      val q = for (c <- coffees) yield c.name
      val a = q.result
      val p: DatabasePublisher[String] = db.stream(a)

      // .foreach is a convenience method on DatabasePublisher.
      // Use Akka Streams for more elaborate stream processing.
      //#stream
      val f =
      //#stream
      p.foreach { s => println(s"Element: $s") }
      //#stream
      Await.result(f, Duration.Inf)
    };{
      //#streamblob
      val q = for (c <- coffees) yield c.image
      val a = q.result
      val p1: DatabasePublisher[Blob] = db.stream(a)
      val p2: DatabasePublisher[Array[Byte]] = p1.mapResult { b =>
        // Executed synchronously on the database thread
        b.getBytes(0, b.length().toInt)
      }
      //#streamblob
    };{
      //#transaction
      val a = (for {
        ns <- coffees.filter(_.name.startsWith("ESPRESSO")).map(_.name).result
        _ <- DBIO.seq(ns.map(n => coffees.filter(_.name === n).delete): _*)
      } yield ()).transactionally

      val f: Future[Unit] = db.run(a)
      //#transaction
      Await.result(f, Duration.Inf)
    };{
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
      assert(Await.result(f, Duration.Inf) == "Roll it back")
    }
    lines.foreach(Predef.println _)
  } finally db.close

  //#simpleaction
  val getAutoCommit = SimpleDBIO[Boolean](_.connection.getAutoCommit)
  //#simpleaction
}

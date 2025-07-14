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
import slick.jdbc.H2Profile
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

  // Advanced transaction control examples
  if (false) {
    val db = Database.forConfig("mydb")

    //#manual-transaction
    // Manual transaction management
    val manualTransaction = for {
      _ <- H2Profile.unsafeBeginTransaction  // Start transaction, pin session
      _ <- coffees.schema.create
      inTx <- H2Profile.isInTransaction     // Check transaction state
      _ <- if (true) H2Profile.unsafeCommitTransaction
           else H2Profile.unsafeRollbackTransaction
    } yield inTx

    db.run(manualTransaction)
    //#manual-transaction

    //#session-pinning
    // Manual session pinning for performance optimization
    val batchedOperations = for {
      _ <- H2Profile.unsafePinSession       // Pin session
      _ <- coffees.schema.create            // Operations reuse same connection
      _ <- coffees += ("Colombian", new SerialBlob("image".getBytes))
      pinned <- H2Profile.isSessionPinned   // Check pinning state
      _ <- H2Profile.unsafeUnpinSession     // Unpin session
    } yield pinned

    db.run(batchedOperations)
    //#session-pinning

    //#manual-savepoint
    // Manual savepoint management
    val savepointExample = (for {
      _ <- coffees.schema.create
      _ <- coffees += ("House Blend", new SerialBlob("image1".getBytes))
      sp <- H2Profile.unsafeCreateSavepoint("coffee_save")
      _ <-
        (coffees += ("French Roast", new SerialBlob("image2".getBytes)))
          .cleanUp {
            case None     => DBIO.unit
            case Some(ex) => H2Profile.unsafeRollbackToSavepoint(sp) >> DBIO.failed(ex)
          }
      _ <- H2Profile.unsafeReleaseSavepoint(sp)  // Success - clean up savepoint
    } yield ()).transactionally

    db.run(savepointExample)
    //#manual-savepoint

    //#high-level-savepoint
    // High-level savepoint usage
    val safeSavepoint = (for {
      _ <- coffees.schema.create
      _ <- coffees += ("House Blend", new SerialBlob("image1".getBytes))
      _ <- (for {
        _ <- coffees += ("French Roast", new SerialBlob("image2".getBytes))  // Protected by savepoint
        _ <- DBIO.failed(new RuntimeException("Simulated failure"))         // This might fail
      } yield ()).withSavepoint("attempt")   // Auto-rollback on failure
      _ <- coffees += ("Success Coffee", new SerialBlob("image3".getBytes))
    } yield ()).transactionally

    db.run(safeSavepoint)
    //#high-level-savepoint

    db.close
  }
}

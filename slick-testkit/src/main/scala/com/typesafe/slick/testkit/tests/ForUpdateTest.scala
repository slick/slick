package com.typesafe.slick.testkit.tests

import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit, ThreadPoolExecutor}

import com.typesafe.slick.testkit.util.{TestkitConfig, AsyncTest, JdbcTestDB}
import org.junit.Assert
import slick.dbio.DBIOAction
import slick.jdbc.{SQLServerProfile, TransactionIsolation}
import slick.util.Logging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Failure


class ForUpdateTest extends AsyncTest[JdbcTestDB] with Logging {
  import tdb.profile.api._

  val tableName = "test_forupdate"
  class T(tag: Tag) extends Table[(Int, Option[String])](tag, tableName) {
    def id = column[Int]("id", O.PrimaryKey)
    def data = column[Option[String]]("data")
    def * = (id, data)
  }
  val ts = TableQuery[T]
  def testForUpdate: DBIO[Unit] = {
    ifCap(jcap.forUpdate) {
      val exe = new ThreadPoolExecutor(2, 2, 1L, TimeUnit.SECONDS, new LinkedBlockingQueue[Runnable]())
      @volatile var success = true
      val childStartLatch = new CountDownLatch(1)
      val thread1Latch = new CountDownLatch(1)
      val thread2Latch = new CountDownLatch(1)
      val rowSelect: (Int) => Query[T, (Int, Option[String]), Seq] = (i: Int) => ts.filter(_.id === i)
      def runAsyncCommands[T](commands: DBIO[Unit], latch: CountDownLatch): DBIO[Unit] = {
        exe.execute(new Runnable {
          override def run(): Unit = Await.result({
            childStartLatch.await()
            val f = db.run {
              seq(GetTransactionality.map(_._1 shouldBe 0), // make sure not in transaction yet
                commands.transactionally)
            }
            f.onComplete { t => {
              latch.countDown()
              t match {
                case Failure(e) => success = false
                case _ =>
              }
            }}
            f
          }, TestkitConfig.asyncTimeout)
        })
        DBIOAction.successful(()) // dummy action to add into pipeline
      }
      seq(
        ifCap(tcap.selectForUpdateRowLocking) { // if database is capable of executing a row locking test
          for {
            _ <- ts.schema.create
            _ <- ts ++= Seq((1, None), (2, None))
            // start txn for main thread
            _ <- (for {
              _ <- GetTransactionality.map(_._1 shouldBe 1) // check in main transaction
              // locking read on row.id 1
              r1 <- rowSelect(1).forUpdate.result
              _ = r1 shouldBe Seq((1, None))
              _ <- runAsyncCommands(seq(
                                      // this read is free to continue
                                      rowSelect(2).forUpdate.result.map(x => x shouldBe Seq((2, None))),
                                      rowSelect(2).map(_.data).update(Some("Thread 1 update"))
                                    ), thread1Latch)
              _ <- runAsyncCommands(seq(
                                      // this read blocks on main thread txn,so check the main update happened first once running
                                      rowSelect(1).forUpdate.result.map(_ shouldBe Seq((1, Some("Main thread update")))),
                                      rowSelect(1).map(_.data).update(Some("Thread 2 update"))
                                    ), thread2Latch)
              _ = childStartLatch.countDown() // start child threads
              _ = thread1Latch.await() // wait for thread 1 to finish
              _ <- rowSelect(1).map(_.data).update(Some("Main thread update"))
            } yield ()).transactionally
            _ = thread2Latch.await()
            // Thread 2 update should have overwritten main thread update
            _ <- ts.result.map(_.toSet shouldBe Set((1, Some("Thread 2 update")), (2, Some("Thread 1 update"))))
            _ <- ts.schema.drop
            _ = exe.shutdown()
            // Fail the test if there were failures in the child threads
            _ = Assert.assertTrue(success)
          } yield ()
        },
        ifNotCap(tcap.selectForUpdateRowLocking) { // a simple test to assert the syntax is valid
          for {
            _ <- ts.schema.create
            _ <- ts ++= Seq((1, None), (2, None))
            r1 <- rowSelect(1).forUpdate.result
            _ = r1 shouldBe Seq((1, None))
          } yield ()
        })
    }
  }
}

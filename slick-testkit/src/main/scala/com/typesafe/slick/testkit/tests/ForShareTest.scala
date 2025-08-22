package com.typesafe.slick.testkit.tests

import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit, ThreadPoolExecutor}

import com.typesafe.slick.testkit.util.{TestkitConfig, AsyncTest, JdbcTestDB}
import org.junit.Assert
import slick.dbio.DBIOAction
import slick.util.Logging

import scala.concurrent.Await
import scala.util.Failure


class ForShareTest extends AsyncTest[JdbcTestDB] with Logging {
  import tdb.profile.api._

  val tableName = "test_forshare"
  class T(tag: Tag) extends Table[(Int, Option[String])](tag, tableName) {
    def id = column[Int]("id", O.PrimaryKey)
    def data = column[Option[String]]("data")
    def * = (id, data)
  }
  val ts = TableQuery[T]
  def testForShare: DBIO[Unit] = {
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
              // shared locking read on row.id 1
              r1 <- rowSelect(1).forShare.result
              _ = r1 shouldBe Seq((1, None))
              _ <- runAsyncCommands(seq(
                                      // this read should be able to continue (shared locks are compatible)
                                      rowSelect(1).forShare.result.map(x => x shouldBe Seq((1, None))),
                                      rowSelect(2).forUpdate.result.map(x => x shouldBe Seq((2, None))),
                                      rowSelect(2).map(_.data).update(Some("Thread 1 update"))
                                    ), thread1Latch)
              _ <- runAsyncCommands(seq(
                                      // this shared read should also work (shared locks are compatible)
                                      rowSelect(1).forShare.result.map(x => x shouldBe Seq((1, None)))
                                    ), thread2Latch)
              _ = childStartLatch.countDown() // start child threads
              _ = thread1Latch.await() // wait for thread 1 to finish
              _ = thread2Latch.await() // wait for thread 2 to finish
            } yield ()).transactionally
            // verify the update from thread 1 worked
            _ <- ts.result.map(_.toSet shouldBe Set((1, None), (2, Some("Thread 1 update"))))
            _ <- ts.schema.drop
          } yield ()
        },
        ifNotCap(tcap.selectForUpdateRowLocking) { // a simple test to assert the syntax is valid
          for {
            _ <- ts.schema.create
            _ <- ts ++= Seq((1, None), (2, None))
            r1 <- rowSelect(1).forShare.result
            _ = r1 shouldBe Seq((1, None))
          } yield ()
        })
    }
  }
}
package slick.benchmark

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global

import fs2.interop.reactivestreams._

import org.reactivestreams.tck.TestEnvironment
import slick.cats
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile

object StreamsStressTest {
  /*import slick.jdbc.DerbyProfile.api._
  val url = "jdbc:derby:memory:StreamsStressTest;create=true"
  val driver = "org.apache.derby.jdbc.EmbeddedDriver"*/
  import slick.jdbc.H2Profile.api._
  val url = "jdbc:h2:mem:StreamsStressTest"
  val driver = "org.h2.Driver"

  val repeats = 10000
  val numThreads = 500

  val env = new TestEnvironment(30000)
  val entityNum = new AtomicInteger()

  def main(args: Array[String]): Unit =
    cats.Database.resource(DatabaseConfig.forURL(H2Profile, url, driver = driver, keepAliveConnection = true)).use { db =>
      Dispatcher.parallel[IO].use { implicit dispatcher =>
        IO {
          val threads = 1.to(numThreads).toVector.map { i =>
            new Thread(new Runnable {
              def run(): Unit = {
                try {
                  for(j <- 1 to repeats) {
                    run1(db, dispatcher)
                    if(j % 100 == 0) println(s"Thread $i: Stream $j successful")
                  }
                } catch { case t: Throwable => env.flop(t, t.toString) }
              }
            })
          }
          threads.foreach(_.start())
          threads.foreach(_.join())
          println("All threads finished")
          env.verifyNoAsyncErrors()
        }
      }
    }.unsafeRunSync()

  def run1(db: cats.Database, dispatcher: Dispatcher[IO]): Unit = {
    val sub = env.newManualSubscriber(createPublisher(db, dispatcher, 1L))
    sub.requestNextElementOrEndOfStream("Timeout while waiting for next element from Publisher")
    sub.requestEndOfStream()
  }

  def createPublisher(db: cats.Database, dispatcher: Dispatcher[IO], elements: Long) = {
    val tableName = "data_" + elements + "_" + entityNum.incrementAndGet()
    class Data(tag: Tag) extends Table[Int](tag, tableName) {
      def id = column[Int]("id")
      def * = id
    }
    val data = TableQuery[Data]
    val a = data.schema.create >> (data ++= Range.apply(0, elements.toInt)) >> data.sortBy(_.id).map(_.id).result
    val resource = db.stream(a.withPinnedSession).toUnicastPublisher
    dispatcher.unsafeRunSync(resource.allocated.map(_._1))
  }
}

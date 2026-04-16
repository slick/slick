package slick.benchmark

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile

object StreamsStressTest {
  import slick.jdbc.H2Profile.api._
  val url = "jdbc:h2:mem:StreamsStressTest"
  val driver = "org.h2.Driver"

  val repeats = 10000
  val numThreads = 500

  val entityNum = new AtomicInteger()

  def main(args: Array[String]): Unit =
    Database.resource(DatabaseConfig.forURL(H2Profile, url, driver = driver, keepAliveConnection = true)).use { db =>
      IO {
        val threads = 1.to(numThreads).toVector.map { i =>
          new Thread(new Runnable {
            def run(): Unit = {
              for (j <- 1 to repeats) {
                run1(db)
                if (j % 100 == 0) println(s"Thread $i: Stream $j successful")
              }
            }
          })
        }
        threads.foreach(_.start())
        threads.foreach(_.join())
        println("All threads finished")
      }
    }.unsafeRunSync()

  def run1(db: Database): Unit = {
    val tableName = "data_" + entityNum.incrementAndGet()
    class Data(tag: Tag) extends Table[Int](tag, tableName) {
      def id = column[Int]("id")
      def * = id
    }
    val data = TableQuery[Data]
    val a = data.schema.create >> (data ++= Range(0, 1)) >> data.sortBy(_.id).map(_.id).result
    db.stream(a.withPinnedSession).compile.toVector.unsafeRunSync()
  }
}

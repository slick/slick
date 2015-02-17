package slick.benchmark

import java.util.concurrent.atomic.AtomicInteger

import org.reactivestreams.tck.TestEnvironment

object StreamsStressTest extends App {
  /*import slick.driver.DerbyDriver.api._
  val url = "jdbc:derby:memory:StreamsStressTest;create=true"
  val driver = "org.apache.derby.jdbc.EmbeddedDriver"*/
  import slick.driver.H2Driver.api._
  val url = "jdbc:h2:mem:StreamsStressTest"
  val driver = "org.h2.Driver"

  val repeats = 10000
  val numThreads = 500

  val env = new TestEnvironment(30000)
  val entityNum = new AtomicInteger()
  val db = Database.forURL(url, driver = driver, keepAliveConnection = true)
  try {
    val threads = 1.to(numThreads).toVector.map { i =>
      new Thread(new Runnable {
        def run(): Unit = {
          try {
            for(j <- 1 to repeats) {
              run1
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
  } finally db.close

  def run1: Unit = {
    val sub = env.newManualSubscriber(createPublisher(1L))
    sub.requestNextElementOrEndOfStream("Timeout while waiting for next element from Publisher")
    sub.requestEndOfStream()
  }

  def createPublisher(elements: Long) = {
    val tableName = "data_" + elements + "_" + entityNum.incrementAndGet()
    class Data(tag: Tag) extends Table[Int](tag, tableName) {
      def id = column[Int]("id")
      def * = id
    }
    val data = TableQuery[Data]
    val a = data.schema.create >> (data ++= Range.apply(0, elements.toInt)) >> data.sortBy(_.id).map(_.id).result
    db.stream(a.withPinnedSession)
  }
}

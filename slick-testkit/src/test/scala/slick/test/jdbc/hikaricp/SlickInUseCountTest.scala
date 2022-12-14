package slick.test.jdbc.hikaricp

import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import slick.jdbc.H2Profile.api._
import slick.lifted.{ProvenShape, TableQuery}
import slick.util.{ManagedArrayBlockingQueue, SlickLogger}

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import javax.management.ObjectName
import org.junit.Assert.assertEquals
import org.junit.{After, Before, Test}
import org.slf4j.LoggerFactory

class SlickInUseCountTest extends AsyncTest[JdbcTestDB] {

  val poolName = "inUseCount"
  val mBeanServer = ManagementFactory.getPlatformMBeanServer
  val aeBeanName = new ObjectName(s"slick:type=AsyncExecutor,name=$poolName")
  val poolBeanName = new ObjectName(s"com.zaxxer.hikari:type=Pool ($poolName)")

  val logger = new SlickLogger(LoggerFactory.getLogger("slick.util.AsyncExecutor"))

  class TestTable(tag: Tag) extends Table[Int](tag, "SDL") {

    def id: Rep[Int] = column[Int]("ID")
    def * : ProvenShape[Int] = id

  }

  var database: Database = _
  val testTable: TableQuery[TestTable] = TableQuery[TestTable]

  @Before
  def openDatabase() = {
    System.setProperty("com.zaxxer.hikari.housekeeping.periodMs", "5000")
    database = Database.forConfig("h2mem-inuse")
    Await.result(database.run(testTable.schema.create), Duration.Inf /*2.seconds*/)
  }

  @After
  def closeDatabase() = {
    Await.result(database.run(testTable.schema.drop), 2.seconds)
    database.close()
  }

  @Test def slickInUseCount(): Unit = {
    val loops = 10 // 1000
    val count = 100
    1 to loops foreach { _ =>
      val tasks = 1 to count map { i =>
        val action = { testTable += i }
          .flatMap { _ => testTable.length.result }
          //.flatMap { _ => DBIO.successful(s"inserted value $i") }

        database.run(action)
      }
      Await.result(Future.sequence(tasks), Duration(10, TimeUnit.SECONDS))

    }
    //we need to wait until there are no more active threads in the thread pool
    //DBIOAction results might be available before the threads have completely finished their work
    while (mBeanServer.getAttribute(aeBeanName, "ActiveThreads").asInstanceOf[Int] > 0) {
      Thread.sleep(100)
    }

    val asyncExecutor = database.executor.asInstanceOf[AsyncExecutor.DefaultAsyncExecutor]
    val queue = asyncExecutor.queue.asInstanceOf[ManagedArrayBlockingQueue]

    assertEquals(0, queue.nonHighItemsInUseCount)
  }
}

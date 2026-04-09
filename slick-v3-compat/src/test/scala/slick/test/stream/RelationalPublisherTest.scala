package slick.test.stream

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import slick.relational.RelationalProfile

import org.reactivestreams.{Publisher, Subscriber}
import org.reactivestreams.tck.*
import org.scalatestplus.testng.TestNGSuiteLike
import org.testng.annotations.{AfterClass, BeforeClass}


abstract class RelationalPublisherTest[P <: RelationalProfile](val profile: P, timeout: Long)
  extends PublisherVerification[Int](new TestEnvironment(timeout), 1000L) with TestNGSuiteLike {

  import profile.api.*


  override def maxElementsFromPublisher = 73L
  override def boundedDepthOfOnNextAndRequestRecursion = 1L

  class Data(tableName: String)(tag: Tag) extends Table[Int](tag, tableName) {
    def id = column[Int]("id")
    def * = id
  }
  lazy val data = TableQuery(new Data("data")(_))
  lazy val dataErr = TableQuery(new Data("data_err")(_))

  var db: slick.compat.Database = _
  val entityNum = new AtomicInteger()

  def createDB: slick.compat.Database

  @BeforeClass def setUpDB(): Unit = {
    db = createDB
    Await.result(db.run(data.schema.create >> (data ++= (1 to maxElementsFromPublisher.toInt))), Duration.Inf)
  }

  @AfterClass def tearDownDB(): Unit = {
    db.close()
  }

  def createPublisher(elements: Long): Publisher[Int] = {
    db.stream(data.filter(_.id <= elements.toInt).sortBy(_.id).result)
  }

  /** Returns a publisher that immediately signals `onError` on every subscriber.
    *
    * The TCK may call `subscribe()` on the returned publisher multiple times.
    * FS2's `StreamUnicastPublisher` may not signal `onError` quickly enough within the TCK timeout,
    * so we use a synchronous publisher that immediately calls `onError` after `onSubscribe`.
    */
  def createFailedPublisher: Publisher[Int] = new Publisher[Int] {
    def subscribe(subscriber: Subscriber[_ >: Int]): Unit = {
      if (subscriber == null) throw new NullPointerException("Subscriber must not be null")
      // Per Reactive Streams spec, must call onSubscribe before onError
      subscriber.onSubscribe(new org.reactivestreams.Subscription {
        def request(n: Long): Unit = ()
        def cancel(): Unit = ()
      })
      subscriber.onError(new RuntimeException("Test: data_err table does not exist"))
    }
  }
}

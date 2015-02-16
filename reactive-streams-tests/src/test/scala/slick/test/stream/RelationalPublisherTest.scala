package slick.test.stream

import java.util.concurrent.atomic.AtomicInteger

import org.reactivestreams._
import org.reactivestreams.tck._
import org.testng.annotations.{AfterClass, BeforeClass}

import slick.profile.RelationalProfile

abstract class RelationalPublisherTest[P <: RelationalProfile](val driver: P, timeout: Long) extends PublisherVerification[Int](new TestEnvironment(timeout), 1000L) {
  import driver.api._

  var db: Database = _
  val entityNum = new AtomicInteger()

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

  def createErrorStatePublisher = {
    val p = createPublisher(0)
    p.subscribe(new Subscriber[Int] {
      def onSubscribe(s: Subscription): Unit = s.cancel
      def onComplete(): Unit = ()
      def onError(t: Throwable): Unit = ()
      def onNext(t: Int): Unit = ()
    })
    p
  }

  override def maxElementsFromPublisher = 73L

  override def boundedDepthOfOnNextAndRequestRecursion = 1
}

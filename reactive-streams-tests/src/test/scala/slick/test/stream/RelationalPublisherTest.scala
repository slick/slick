package slick.test.stream

import java.util.concurrent.atomic.AtomicInteger

import org.reactivestreams._
import org.reactivestreams.tck._
import org.testng.annotations.{AfterClass, BeforeClass}
import org.scalatest.testng.TestNGSuiteLike

import slick.profile.RelationalProfile

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

abstract class RelationalPublisherTest[P <: RelationalProfile](val driver: P, timeout: Long) extends PublisherVerification[Int](new TestEnvironment(timeout), 1000L) with TestNGSuiteLike {
  import driver.api._

  override def maxElementsFromPublisher = 73L
  override def boundedDepthOfOnNextAndRequestRecursion = 1

  class Data(tableName: String)(tag: Tag) extends Table[Int](tag, tableName) {
    def id = column[Int]("id")
    def * = id
  }
  lazy val data = TableQuery(new Data("data")(_))
  lazy val dataErr = TableQuery(new Data("data_err")(_))

  var db: Database = _
  val entityNum = new AtomicInteger()

  def createDB: Database

  @BeforeClass def setUpDB: Unit = {
    db = createDB
    Await.result(db.run(data.schema.create >> (data ++= (1 to maxElementsFromPublisher.toInt))), Duration.Inf)
  }

  @AfterClass def tearDownDB: Unit =
    db.close()

  def createPublisher(elements: Long) =
    db.stream(data.filter(_.id <= elements.toInt).sortBy(_.id).result)

  def createFailedPublisher =
    db.stream(dataErr.result)
}

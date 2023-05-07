package slick.test.stream

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import slick.relational.RelationalProfile

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

  var db: Database = _
  val entityNum = new AtomicInteger()

  def createDB: Database

  @BeforeClass def setUpDB(): Unit = {
    db = createDB
    Await.result(db.run(data.schema.create >> (data ++= (1 to maxElementsFromPublisher.toInt))), Duration.Inf)
  }

  @AfterClass def tearDownDB(): Unit =
    db.close()

  def createPublisher(elements: Long) =
    db.stream(data.filter(_.id <= elements.toInt).sortBy(_.id).result)

  def createFailedPublisher =
    db.stream(dataErr.result)
}

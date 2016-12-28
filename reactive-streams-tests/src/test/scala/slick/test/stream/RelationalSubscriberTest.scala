package slick.test.stream

import org.reactivestreams.Subscriber
import org.reactivestreams.tck._
import org.scalatest.testng.TestNGSuiteLike
import org.testng.annotations.{AfterClass, BeforeClass}
import slick.basic.DatabaseSubscriber
import slick.relational.RelationalProfile

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

abstract class RelationalSubscriberTest[P <: RelationalProfile](val profile: P, timeout: Long) extends SubscriberBlackboxVerification[slick.dbio.DBIO[Int]](new TestEnvironment(timeout)) with TestNGSuiteLike {
  import profile.api._

  class Data(tableName: String)(tag: Tag) extends Table[Int](tag, tableName) {
    def id = column[Int]("id")
    def * = id
  }
  lazy val data = TableQuery(new Data("data")(_))

  var db: Database = _
  def createDB: Database

  @BeforeClass def setUpDB: Unit = {
    db = createDB
    Await.result(db.run(data.schema.create), Duration.Inf)
  }

  @AfterClass def tearDownDB: Unit =
    db.close()

  override def createSubscriber(): Subscriber[DBIO[Int]] = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    new DatabaseSubscriber[Int,P](db)
  }

  override def createElement(element: Int): DBIO[Int] = {
    DBIO.from(Future.successful(element))
  }
}

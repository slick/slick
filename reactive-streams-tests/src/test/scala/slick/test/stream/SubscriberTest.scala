package slick.test.stream

import slick.jdbc.{H2Profile, JdbcProfile}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

class SubscriberTest extends RelationalSubscriberTest[JdbcProfile](H2Profile, 1000L) {
  import profile.api._

  def createDB = {
    val db = Database.forURL("jdbc:h2:mem:SubscriberTest", driver = "org.h2.Driver", keepAliveConnection = true)
    // Wait until the database has been initialized and can process queries:
    try { Await.result(db.run(sql"select 1".as[Int]), Duration.Inf) } catch { case NonFatal(ex) => }
    db
  }
}

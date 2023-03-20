package slick.test.stream

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

import slick.jdbc.{H2Profile, JdbcProfile}


class JdbcPublisherTest extends RelationalPublisherTest[JdbcProfile](H2Profile, 1000L) {

  import profile.api.*


  def createDB = {
    val db = Database.forURL("jdbc:h2:mem:DatabasePublisherTest", driver = "org.h2.Driver", keepAliveConnection = true)
    // Wait until the database has been initialized and can process queries:
    try {
      Await.result(db.run(sql"SELECT 1".as[Int]), Duration.Inf)
    } catch {
      case NonFatal(ex) =>
    }
    db
  }
}

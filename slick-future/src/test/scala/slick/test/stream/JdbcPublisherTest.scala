package slick.test.stream

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import slick.future.Database
import slick.jdbc.{DatabaseConfig, H2Profile, JdbcProfile}


class JdbcPublisherTest extends RelationalPublisherTest[JdbcProfile](H2Profile, 1000L) {

  val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:DatabasePublisherTest", driver = "org.h2.Driver", keepAliveConnection = true)

  def createDB = Await.result(Database.open(dc), Duration.Inf)
}

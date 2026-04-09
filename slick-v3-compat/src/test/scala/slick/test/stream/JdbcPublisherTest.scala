package slick.test.stream

import cats.effect.unsafe.implicits.global

import slick.jdbc.{H2Profile, JdbcProfile}


class JdbcPublisherTest extends RelationalPublisherTest[JdbcProfile](H2Profile, 1000L) {
  import profile.api.*

  def createDB: slick.compat.Database = {
    slick.compat.Database.wrap(Database.forURL("jdbc:h2:mem:DatabasePublisherTest", driver = "org.h2.Driver", keepAliveConnection = true))
  }
}

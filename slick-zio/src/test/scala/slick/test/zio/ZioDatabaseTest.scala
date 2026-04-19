package slick.test.zio

import com.typesafe.config.ConfigFactory

import zio.ZIO
import zio.test.{ZIOSpecDefault, assertTrue}

import slick.jdbc.{DatabaseConfig, JdbcProfile}
import slick.zio.Database

class ZioDatabaseTest extends ZIOSpecDefault {

  private val h2Config = ConfigFactory.parseString(
    """
      |mydb {
      |  profile = "slick.jdbc.H2Profile$"
      |  db {
      |    connectionPool = disabled
      |    driver = "org.h2.Driver"
      |    url = "jdbc:h2:mem:zioapitest;DB_CLOSE_DELAY=-1"
      |  }
      |}
      |""".stripMargin
  )

  override def spec =
    suite("slick.zio.Database")(
      test("run and stream execute DBIO actions") {
        val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
        import dc.profile.api.*

        class Rows(tag: Tag) extends Table[Int](tag, "ZIO_ROWS") {
          def v = column[Int]("V")
          def * = v
        }
        val rows = TableQuery[Rows]

        ZIO.scoped {
          for {
            db <- Database.scoped(dc)
            _ <- db.run(rows.schema.create)
            _ <- db.run(DBIO.sequence((1 to 5).map(rows += _)))
            runResult <- db.run(rows.sortBy(_.v).result)
            streamResult <- db.stream(rows.sortBy(_.v).result).runCollect
            _ <- db.run(rows.schema.drop)
          } yield assertTrue(runResult == Vector(1, 2, 3, 4, 5), streamResult.toVector == Vector(1, 2, 3, 4, 5))
        }
      }
    )
}

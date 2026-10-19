package slick.test.zio

import com.typesafe.config.ConfigFactory

import java.util.concurrent.atomic.AtomicInteger

import zio.{Task, ZIO}
import zio.interop.catz.*
import zio.test.{ZIOSpecDefault, assertTrue}

import slick.basic.ActionListener
import slick.jdbc.{DatabaseConfig, JdbcProfile}
import slick.zio.Database

object ZioDatabaseTest extends ZIOSpecDefault {

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
            result <- (
              for {
                _ <- db.run(rows.schema.dropIfExists)
                _ <- db.run(rows.schema.createIfNotExists)
                _ <- db.run(DBIO.sequence((1 to 5).map(rows += _)))
                runResult <- db.run(rows.sortBy(_.v).result)
                streamResult <- db.stream(rows.sortBy(_.v).result).runCollect
              } yield assertTrue(runResult == Vector(1, 2, 3, 4, 5), streamResult.toVector == Vector(1, 2, 3, 4, 5))
            ).ensuring(db.run(rows.schema.dropIfExists).ignore)
          } yield result
        }
      },
      test("an ActionListener can be installed through the core API") {
        val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
        import dc.profile.api.*
        val seen = new AtomicInteger
        val counting = new ActionListener[Task] {
          override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: Task[H]): Task[H] =
            ZIO.succeed(seen.incrementAndGet()) *> exec
        }
        ZIO.scoped {
          for {
            db <- ZIO.acquireRelease(
              dc.profile.backend.makeDatabase[Task](dc, ActionListener.combine(ActionListener.timing[Task], counting))
                .map(Database.fromCore)
            )(db => ZIO.attemptBlocking(db.close()).ignore)
            r <- db.run(sql"select 1".as[Int].head)
          } yield assertTrue(r == 1, seen.get > 0)
        }
      }
    )
}

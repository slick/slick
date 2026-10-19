package slick.test.zio

import com.typesafe.config.ConfigFactory

import java.util.concurrent.atomic.AtomicInteger

import zio.{durationInt, Task, ZIO}
import zio.interop.catz.*
import zio.test.{ZIOSpecDefault, assertTrue}

import slick.basic.ActionListener
import slick.jdbc.{DatabaseConfig, GetResult, JdbcProfile}
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

  /** Recurses until the stack overflows. */
  private def overflow(n: Int): Int = if (n < 0) n else overflow(n + 1) + 1

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
      },
      test("a StackOverflowError while reading the rows of a stream fails the stream") {
        val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
        import dc.profile.api.*
        val action = {
          implicit val overflowingRow: GetResult[Int] = GetResult(_ => overflow(0))
          sql"select x from system_range(1, 3)".as[Int]
        }
        def isOverflow(t: Throwable) = t match {
          case e: SlickException => e.getCause.isInstanceOf[StackOverflowError]
          case _                 => false
        }
        ZIO.scoped {
          for {
            core <- ZIO.acquireRelease(dc.makeDatabase[Task]())(core => ZIO.attemptBlocking(core.close()).ignore)
            db = Database.fromCore(core)
            // Read the core iterator outside of any fiber first: an unguarded StackOverflowError in
            // the facade's stream would stop the fiber and hang the test.
            direct <- ZIO.acquireRelease(core.stream(action).allocated)(_._2.ignore).flatMap { case (it, _) =>
              ZIO.attemptBlocking {
                try { it.hasNext; it.next(); None }
                catch { case t: Throwable => Some(t) }
              }
            }
            result <-
              if (direct.exists(isOverflow)) db.stream(action).runDrain.either.timeout(30.seconds)
              else ZIO.none
            after <- db.run(sql"select 1".as[Int].head).timeout(30.seconds)
          } yield assertTrue(
            direct.exists(isOverflow),
            result.exists(_.left.exists(isOverflow)),
            after.contains(1)
          )
        }
      }
    )
}

package slick.test.ce

import cats.effect.IO
import cats.effect.kernel.Outcome
import munit.CatsEffectSuite

import slick.basic.ActionListener
import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*
import slick.sql.SqlAction

class ActionListenerTest extends CatsEffectSuite {

  class Items(tag: Tag) extends Table[Int](tag, "TRACE_ITEMS") {
    def v = column[Int]("V")
    def * = v
  }
  val items = TableQuery[Items]

  private def resourceWithListener(listener: ActionListener[IO]) =
    cats.effect.Resource.make(
      IO.blocking {
        val dc = DatabaseConfig.forConfig[H2Profile](
          "mydb",
          com.typesafe.config.ConfigFactory.parseString(
            """
              |mydb {
              |  profile = "slick.jdbc.H2Profile$"
              |  db {
              |    connectionPool = disabled
              |    driver = "org.h2.Driver"
              |    url = "jdbc:h2:mem:actionlistener;DB_CLOSE_DELAY=-1"
              |  }
              |}
              |""".stripMargin
          )
        )
        Database.fromCore(dc.profile.backend.makeDatabase[IO](dc, listener).unsafeRunSync())
      }
    )(db => IO.blocking(db.close()).attempt.void)

  test("SqlAction exposes SQL statements to listener") {
    for {
      seen <- IO.ref(Vector.empty[String])
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, _, _], exec: IO[H]): IO[H] = {
          val sqls = action match {
            case s: SqlAction[?, ?, ?] => s.statements.toVector
            case _ => Vector.empty
          }
          seen.update(_ ++ sqls) >> exec
        }
      }
      out <- resourceWithListener(listener).use { db =>
        db.run(sql"select 1".as[Int].head)
      }
      sqls <- seen.get
      _ = assertEquals(out, 1)
      _ = assert(sqls.exists(_.toLowerCase.contains("select 1")))
    } yield ()
  }

  test("NamedAction name is visible to listener") {
    for {
      names <- IO.ref(Vector.empty[String])
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, _, _], exec: IO[H]): IO[H] =
          action match {
            case slick.dbio.NamedAction(_, n) => names.update(_ :+ n) >> exec
            case _ => exec
          }
      }
      _ <- resourceWithListener(listener).use { db =>
        db.run(DBIO.successful(1).named("named-node")).void
      }
      seen <- names.get
      _ = assert(seen.contains("named-node"))
    } yield ()
  }

  test("flatMap composition sees both nodes") {
    for {
      count <- IO.ref(0)
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, _, _], exec: IO[H]): IO[H] =
          action match {
            case slick.dbio.NamedAction(_, "left") | slick.dbio.NamedAction(_, "right") =>
              count.update(_ + 1) >> exec
            case _ => exec
          }
      }
      _ <- resourceWithListener(listener).use { db =>
        val a = DBIO.successful(1).named("left")
        val b = (_: Int) => DBIO.successful(2).named("right")
        db.run(a.flatMap(b)).void
      }
      n <- count.get
      _ = assertEquals(n, 2)
    } yield ()
  }

  test("streaming actions are intercepted") {
    for {
      hits <- IO.ref(0)
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, _, _], exec: IO[H]): IO[H] =
          action match {
            case _: SqlAction[?, ?, ?] => hits.update(_ + 1) >> exec
            case _ => exec
          }
      }
      _ <- resourceWithListener(listener).use { db =>
        for {
          _ <- db.run(items.schema.create)
          _ <- db.run(DBIO.sequence((1 to 3).map(items += _)))
          _ <- db.stream(items.result).compile.toVector
          _ <- db.run(items.schema.drop)
        } yield ()
      }
      n <- hits.get
      _ = assert(n > 0)
    } yield ()
  }

  test("around wraps execution with before/after") {
    for {
      events <- IO.ref(Vector.empty[String])
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, _, _], exec: IO[H]): IO[H] =
          events.update(_ :+ "before") >>
            exec.guaranteeCase {
              case Outcome.Succeeded(_) => events.update(_ :+ "after")
              case _ => events.update(_ :+ "after")
            }
      }
      _ <- resourceWithListener(listener).use { db =>
        db.run(DBIO.successful(1)).void
      }
      ev <- events.get
      _ = assert(ev.contains("before"))
      _ = assert(ev.contains("after"))
      _ = assert(ev.indexOf("before") < ev.indexOf("after"))
    } yield ()
  }

  test("noop listener works transparently") {
    resourceWithListener(ActionListener.noop[IO]).use { db =>
      db.run(DBIO.successful(42)).map(v => assertEquals(v, 42))
    }
  }

  test("transactionally exposes TransactionalAction") {
    for {
      seen <- IO.ref(false)
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, _, _], exec: IO[H]): IO[H] =
          action match {
            case _: slick.dbio.TransactionalAction[?, ?, ?] => seen.set(true) >> exec
            case _ => exec
          }
      }
      _ <- resourceWithListener(listener).use { db =>
        db.run(DBIO.successful(()).transactionally).void
      }
      ok <- seen.get
      _ = assert(ok)
    } yield ()
  }
}

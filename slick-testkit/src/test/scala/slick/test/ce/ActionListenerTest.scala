package slick.test.ce

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.kernel.Outcome
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import munit.CatsEffectSuite
import org.slf4j.LoggerFactory

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

  private def dbConfig = DatabaseConfig.forConfig[H2Profile](
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

  private def resourceWithListener(listener: ActionListener[IO]) =
    cats.effect.Resource.make(
      IO.blocking {
        val dc = dbConfig
        Database.fromCore(dc.profile.backend.makeDatabase[IO](dc, listener).unsafeRunSync())
      }
    )(db => IO.blocking(db.close()).attempt.void)

  /** Run `io` with the given loggers at `level` and return its result and the messages they logged. */
  private def withLog[A](level: Level, loggerNames: String*)(io: IO[A]): IO[(A, Vector[String])] = {
    val loggers = loggerNames.map(n => LoggerFactory.getLogger(n).asInstanceOf[Logger])
    val acquire = IO {
      val appender = new ListAppender[ILoggingEvent]
      appender.start()
      val oldLevels = loggers.map(_.getLevel)
      loggers.foreach { l => l.setLevel(level); l.addAppender(appender) }
      (appender, oldLevels)
    }
    cats.effect.Resource.make(acquire) { case (appender, oldLevels) =>
      IO(loggers.zip(oldLevels).foreach { case (l, old) => l.detachAppender(appender); l.setLevel(old) })
    }.use { case (appender, _) =>
      io.map(a => (a, appender.list.asScala.toVector.map(_.getFormattedMessage)))
    }
  }

  private val actionLogger = "slick.basic.BasicBackend.action"
  private val timingLogger = "slick.basic.BasicBackend.timing"
  private def withTimingLog[A](io: IO[A]) = withLog(Level.DEBUG, timingLogger)(io)

  private val timingLine = """SQL action (took|failed after) \d+\.\d{3} ms: (.*)""".r

  test("default listener logs the execution time of SQL actions with their statements") {
    val select = sql"select 41 + 1".as[Int].head
    val failing = sql"select * from NO_SUCH_TABLE".as[Int].head
    withTimingLog(Database.resource(dbConfig).use { db =>
      db.run(select).flatMap(r => db.run(failing).attempt.map(e => (r, e.isLeft)))
    }).map { case ((r, failed), lines) =>
      assertEquals(r, 42)
      assert(failed)
      val logged = lines.collect { case timingLine(outcome, sql) => (outcome, sql) }
      assertEquals(logged, Vector(("took", select.statements.head), ("failed after", failing.statements.head)))
    }
  }

  test("nothing is timed when the timing logger is not at debug level") {
    withLog(Level.INFO, timingLogger)(Database.resource(dbConfig).use(_.run(sql"select 1".as[Int].head))).map {
      case (r, lines) =>
        assertEquals(r, 1)
        assertEquals(lines, Vector.empty[String])
    }
  }

  test("programmatic configs use the default listener") {
    val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:actionlistenerurl", driver = "org.h2.Driver")
    val select = sql"select 2".as[Int].head
    withLog(Level.DEBUG, actionLogger, timingLogger)(Database.resource(dc).use(_.run(select))).map {
      case (r, lines) =>
        assertEquals(r, 2)
        assert(lines.exists(l => l.startsWith("SQL action took ") && l.endsWith(": select 2")), lines.toString)
        assert(lines.exists(l => !l.startsWith("SQL action ") && l.contains("select 2")), lines.toString)
    }
  }

  test("timing listener can be combined with an installed listener") {
    val listener = ActionListener.combine(ActionListener.noop[IO], ActionListener.timing[IO])
    withTimingLog(resourceWithListener(listener).use(_.run(sql"select 3".as[Int].head))).map { case (r, lines) =>
      assertEquals(r, 3)
      assert(lines.exists(l => l.startsWith("SQL action took ") && l.endsWith(": select 3")), lines.toString)
    }
  }

  test("an installed listener replaces the default one") {
    withTimingLog(resourceWithListener(ActionListener.noop[IO]).use(_.run(sql"select 4".as[Int].head))).map {
      case (r, lines) =>
        assertEquals(r, 4)
        assertEquals(lines, Vector.empty[String])
    }
  }

  test("heap databases use the given listener") {
    val dc = slick.basic.DatabaseConfig.forProfileConfig(
      slick.memory.MemoryProfile, "heap", com.typesafe.config.ConfigFactory.parseString("heap {}"))
    for {
      names <- IO.ref(Vector.empty[String])
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
          action match {
            case slick.dbio.NamedAction(_, n) => names.update(_ :+ n) >> exec
            case _ => exec
          }
      }
      r <- cats.effect.Resource.make(dc.profile.backend.makeDatabase[IO](dc, listener).map(Database.fromCore))(db =>
          IO(db.close())).use(_.run(DBIO.successful(5).named("heap-node")))
      seen <- names.get
      _ = assertEquals(r, 5)
      _ = assertEquals(seen, Vector("heap-node"))
    } yield ()
  }

  test("combine runs the first listener outermost") {
    def recording(name: String, events: cats.effect.Ref[IO, Vector[String]]) = new ActionListener[IO] {
      override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
        action match {
          case _: SqlAction[?, ?, ?] =>
            events.update(_ :+ s"$name before") >> exec.guarantee(events.update(_ :+ s"$name after"))
          case _ => exec
        }
    }
    for {
      events <- IO.ref(Vector.empty[String])
      _ <- resourceWithListener(ActionListener.combine(recording("a", events), recording("b", events)))
        .use(_.run(sql"select 1".as[Int].head))
      ev <- events.get
      _ = assertEquals(ev, Vector("a before", "b before", "b after", "a after"))
    } yield ()
  }

  test("SqlAction exposes SQL statements to listener") {
    for {
      seen <- IO.ref(Vector.empty[String])
      listener = new ActionListener[IO] {
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] = {
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
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
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
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
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
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
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
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
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
        override def around[R, H](action: slick.dbio.DBIOAction[R, ?, ?], exec: IO[H]): IO[H] =
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

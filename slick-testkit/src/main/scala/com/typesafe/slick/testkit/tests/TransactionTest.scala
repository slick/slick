package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.tests.TransactionTest.ExpectedException
import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import java.sql.Connection

import scala.annotation.nowarn

import cats.effect.{IO, Resource}

import slick.jdbc.{DatabaseConfig, TransactionIsolation}

class TransactionTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testTransactions = {
    class T(tag: Tag) extends Table[Int](tag, "t") {
      def a = column[Int]("a", O.PrimaryKey)
      def * = a
    }
    val ts = TableQuery[T]

    val getTI = SimpleDBIO(_.connection.getTransactionIsolation)


    ts.schema.create andThen { // failed transaction
      (for {
        _ <- ts += 1
        _ <- ts.result.map(_ shouldBe Seq(1))
        _ <- GetTransactionality.map(_ shouldBe ((1, false)))
        _ = throw new ExpectedException
      } yield ()).transactionally.failed.map(_ should (_.isInstanceOf[ExpectedException]))
    } andThen {
       ts.result.map(_ shouldBe Nil).andThen(GetTransactionality.map(_ shouldBe ((0, true))))
    } andThen { // successful transaction
      (for {
        _ <- ts += 2
        _ <- ts.result.map(_ shouldBe Seq(2))
        _ <- GetTransactionality.map(_ shouldBe ((1, false)))
      } yield ()).transactionally
    } andThen {
      ts.result.map(_ shouldBe Seq(2))
    } andThen { // nested successful transaction
      (for {
        _ <- ts += 3
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3))
        _ <- GetTransactionality.map(_ shouldBe ((2, false)))
      } yield ()).transactionally.transactionally
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(2, 3))
    } andThen { // failed nested transaction
      (for {
        _ <- ts += 4
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3, 4))
        _ <- GetTransactionality.map(_ shouldBe ((2, false)))
        _ = throw new ExpectedException
      } yield ()).transactionally.transactionally.failed.map(_ should (_.isInstanceOf[ExpectedException]))
    } andThen { // fused successful transaction
      (ts += 5).andThen(ts += 6).transactionally
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6)).andThen(GetTransactionality.map(_ shouldBe ((0, true))))
    } andThen { // fused failed transaction
      (ts += 7).andThen(ts += 6).transactionally.failed
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6)).andThen(GetTransactionality.map(_ shouldBe ((0, true))))
    } andThen { ifCap(tcap.transactionIsolation) {
      (for {
        ti1 <- getTI
        // transactionally(ReadUncommitted) sets the isolation level for the transaction
        _ <- (for {
          _ <- getTI.map(_ should (_ >= TransactionIsolation.ReadUncommitted.intValue))
        } yield ()).transactionally(TransactionIsolation.ReadUncommitted)
        // transactionally(Serializable) sets the isolation level for the transaction
        _ <- (for {
          _ <- getTI.map(_ should (_ >= TransactionIsolation.Serializable.intValue))
        } yield ()).transactionally(TransactionIsolation.Serializable)
        // nested transactionally: outer isolation level wins, inner is ignored
        _ <- (for {
          _ <- getTI.map(_ should (_ >= TransactionIsolation.ReadUncommitted.intValue))
          _ <- (for {
            // still ReadUncommitted (outer wins), not Serializable
            _ <- getTI.map(_ should (_ >= TransactionIsolation.ReadUncommitted.intValue))
          } yield ()).transactionally(TransactionIsolation.Serializable)
        } yield ()).transactionally(TransactionIsolation.ReadUncommitted)
        // withTransactionIsolation compatibility: temporary nested isolation changes are restored
        _ <- {
          @nowarn("cat=deprecation")
          val compat = (for {
            _ <- getTI.map(_ should (_ >= TransactionIsolation.ReadUncommitted.intValue))
            _ <- getTI.withTransactionIsolation(TransactionIsolation.Serializable).map(_ should (_ >= TransactionIsolation.Serializable.intValue))
            _ <- getTI.map(_ should (_ >= TransactionIsolation.ReadUncommitted.intValue))
          } yield ()).withTransactionIsolation(TransactionIsolation.ReadUncommitted)
          compat
        }
        // isolation level is back to default after all transactions
        _ <- getTI.map(_ shouldBe ti1)
      } yield ()).withPinnedSession
    }}
  }

  /** A migration tool (such as Flyway) opens a connection, starts a transaction on it, hands the
    * connection to a migration written with Slick, and then commits or rolls back the migration's
    * work together with its own bookkeeping. */
  def testCallerOwnedConnection: IO[Unit] = {
    class Counters(tag: Tag) extends Table[(String, Int)](tag, "CALLER_OWNED_COUNTERS") {
      def name = column[String]("NAME", O.PrimaryKey)
      def value = column[Int]("VALUE")
      def * = (name, value)
    }
    val counters = TableQuery[Counters]

    def migration(name: String) =
      (counters += ((name, 0)))
        .andThen(counters.filter(_.name === name).map(_.value).update(1))
        .transactionally

    def runMigration(conn: Connection, name: String)(finish: Connection => Unit): IO[Unit] =
      slick.cats.Database.resource(DatabaseConfig.forConnection[tdb.profile.type](tdb.profile, conn))
        .use(_.run(migration(name)))
        .flatMap(_ => IO.blocking(finish(conn)))

    val toolConnection = Resource.make(IO.blocking(rawDb.createSession().conn))(conn => IO.blocking(conn.close()))

    toolConnection.use { conn =>
      for {
        _ <- db.run(counters.schema.create)
        _ <- IO.blocking(conn.setAutoCommit(false))
        _ <- runMigration(conn, "rolled back")(_.rollback())
        _ <- IO.blocking {
          conn.isClosed shouldBe false
          conn.getAutoCommit shouldBe false
        }
        _ <- db.run(counters.result).map(_ shouldBe Nil)
        _ <- runMigration(conn, "committed")(_.commit())
        _ <- db.run(counters.result).map(_ shouldBe Seq(("committed", 1)))
        // Without a transaction open on the connection, `transactionally` commits its own
        _ <- IO.blocking(conn.setAutoCommit(true))
        _ <- runMigration(conn, "auto-committed")(_ => ())
        _ <- IO.blocking(conn.getAutoCommit shouldBe true)
        _ <- db.run(counters.to[Set].result).map(_ shouldBe Set(("committed", 1), ("auto-committed", 1)))
      } yield ()
    }
  }
}

object TransactionTest {
  private class ExpectedException extends RuntimeException
}

package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.tests.TransactionTest.ExpectedException
import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

import slick.jdbc.TransactionIsolation

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
        _ <- GetTransactionality.map(_ shouldBe (1, false))
        _ = throw new ExpectedException
      } yield ()).transactionally.failed.map(_ should (_.isInstanceOf[ExpectedException]))
    } andThen {
       ts.result.map(_ shouldBe Nil) andThen
         GetTransactionality.map(_ shouldBe (0, true))
    } andThen { // successful transaction
      (for {
        _ <- ts += 2
        _ <- ts.result.map(_ shouldBe Seq(2))
        _ <- GetTransactionality.map(_ shouldBe (1, false))
      } yield ()).transactionally
    } andThen {
      ts.result.map(_ shouldBe Seq(2))
    } andThen { // nested successful transaction
      (for {
        _ <- ts += 3
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3))
        _ <- GetTransactionality.map(_ shouldBe (2, false))
      } yield ()).transactionally.transactionally
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(2, 3))
    } andThen { // failed nested transaction
      (for {
        _ <- ts += 4
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3, 4))
        _ <- GetTransactionality.map(_ shouldBe (2, false))
        _ = throw new ExpectedException
      } yield ()).transactionally.transactionally.failed.map(_ should (_.isInstanceOf[ExpectedException]))
    } andThen { // fused successful transaction
      (ts += 5).andThen(ts += 6).transactionally
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6)) andThen
        GetTransactionality.map(_ shouldBe (0, true))
    } andThen { // fused failed transaction
      (ts += 7).andThen(ts += 6).transactionally.failed
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6)) andThen
        GetTransactionality.map(_ shouldBe (0, true))
    } andThen { ifCap(tcap.transactionIsolation) {
      (for {
        ti1 <- getTI
        // transactionally(ReadUncommitted) sets the isolation level for the transaction
        _ <- (for {
          _ <- getTI.map(_ should(_ >= TransactionIsolation.ReadUncommitted.intValue))
        } yield ()).transactionally(TransactionIsolation.ReadUncommitted)
        // transactionally(Serializable) sets the isolation level for the transaction
        _ <- (for {
          _ <- getTI.map(_ should(_ >= TransactionIsolation.Serializable.intValue))
        } yield ()).transactionally(TransactionIsolation.Serializable)
        // nested transactionally: outer isolation level wins, inner is ignored
        _ <- (for {
          _ <- getTI.map(_ should(_ >= TransactionIsolation.ReadUncommitted.intValue))
          _ <- (for {
            // still ReadUncommitted (outer wins), not Serializable
            _ <- getTI.map(_ should(_ >= TransactionIsolation.ReadUncommitted.intValue))
          } yield ()).transactionally(TransactionIsolation.Serializable)
        } yield ()).transactionally(TransactionIsolation.ReadUncommitted)
        // isolation level is back to default after all transactions
        _ <- getTI.map(_ shouldBe ti1)
      } yield ()).withPinnedSession
    }} andThen { // savepoints
      (for {
        _ <- ts += 10
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6, 10))
        // withSavepoint: successful inner action - savepoint released, row stays
        _ <- (ts += 11).withSavepoint
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6, 10, 11))
        // withSavepoint: failed inner action - rolled back to savepoint, surrounding tx continues
        _ <- (for {
          _ <- ts += 12
          _ = throw new ExpectedException
        } yield ()).withSavepoint.failed.map(_ should (_.isInstanceOf[ExpectedException]))
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6, 10, 11))
        // direct createSavepoint / rollbackToSavepoint
        sp <- createSavepoint
        _ <- ts += 13
        _ <- rollbackToSavepoint(sp)
        _ <- ts.to[Set].result.map(_ shouldBe Set(2, 3, 5, 6, 10, 11))
      } yield ()).transactionally
    }
  }
}

object TransactionTest {
  private class ExpectedException extends RuntimeException
}

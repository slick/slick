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
        _ <- (for {
          _ <- getTI.map(_ should(_ >= TransactionIsolation.ReadUncommitted.intValue))
          _ <- getTI.withTransactionIsolation(TransactionIsolation.Serializable).map(_ should(_ >= TransactionIsolation.Serializable.intValue))
          _ <- getTI.map(_ should(_ >= TransactionIsolation.ReadUncommitted.intValue))
        } yield ()).withTransactionIsolation(TransactionIsolation.ReadUncommitted)
        _ <- getTI.map(_ shouldBe ti1)
      } yield ()).withPinnedSession
    }}
  }
}

object TransactionTest {
  private class ExpectedException extends RuntimeException
}

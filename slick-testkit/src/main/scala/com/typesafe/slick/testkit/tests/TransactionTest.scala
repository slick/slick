package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.tests.TransactionTest.ExpectedException
import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

import slick.jdbc.TransactionIsolation

class TransactionTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testHighLevelSavepointAPI = ifCap(jcap.savepoint) {
    class T(tag: Tag) extends Table[Int](tag, "t_hl_savepoint") {
      def a = column[Int]("a", O.PrimaryKey)
      def * = a
    }
    val ts = TableQuery[T]

    ts.schema.create andThen { // test high-level savepoint API with success
      (for {
        _ <- ts += 1000
        _ <- (for {
          _ <- ts += 2000
          _ <- ts.to[Set].result.map(_ shouldBe Set(1000, 2000))
        } yield ()).withSavepoint("inner_scope")
        _ <- ts.to[Set].result.map(_ shouldBe Set(1000, 2000)) // both should be committed
      } yield ()).transactionally
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(1000, 2000)) // transaction committed
    } andThen { // test high-level savepoint API with failure and rollback
      (for {
        _ <- ts += 3000
        _ <- (for {
          _ <- ts += 4000
          _ <- ts.to[Set].result.map(_ shouldBe Set(1000, 2000, 3000, 4000))
          _ = throw new ExpectedException
        } yield ()).withSavepoint("failing_scope").failed.map(_ should (_.isInstanceOf[ExpectedException]))
        _ <- ts.to[Set].result.map(_ shouldBe Set(1000, 2000, 3000)) // 4000 should be rolled back
        _ <- ts += 5000
      } yield ()).transactionally
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(1000, 2000, 3000, 5000)) // final state
    } andThen { // test nested savepoints
      (for {
        _ <- ts += 6000
        _ <- (for {
          _ <- ts += 7000
          _ <- (for {
            _ <- ts += 8000
            _ = throw new ExpectedException
          } yield ()).withSavepoint("inner_inner").failed.map(_ should (_.isInstanceOf[ExpectedException]))
          _ <- ts.to[Set].result.map(_ shouldBe Set(1000, 2000, 3000, 5000, 6000, 7000)) // 8000 rolled back
          _ <- ts += 9000
        } yield ()).withSavepoint("inner")
        _ <- ts += 10000
      } yield ()).transactionally
    } andThen {
      ts.result.map(_.toSet shouldBe Set(1000, 2000, 3000, 5000, 6000, 7000, 9000, 10000))
    } andThen { // test auto-named savepoints
      (for {
        _ <- ts += 11000
        _ <- (for {
          _ <- ts += 12000
          _ = throw new ExpectedException  
        } yield ()).withSavepoint.failed.map(_ should (_.isInstanceOf[ExpectedException]))
        _ <- ts.result.map(_.size shouldBe 9) // 12000 rolled back, size stays the same
      } yield ()).transactionally
    } andThen {
      ts.to[Set].result.map(_ should (_.contains(11000))) // 11000 should be committed
    } andThen {
      ts.schema.drop
    }
  }

  def testUnsafeSavepointPrimitives = ifCap(jcap.savepoint) {
    class T(tag: Tag) extends Table[Int](tag, "t_savepoint") {
      def a = column[Int]("a", O.PrimaryKey)
      def * = a
    }
    val ts = TableQuery[T]

    ts.schema.create andThen { // test savepoints with rollback
      for {
        _ <- tdb.profile.unsafeBeginTransaction
        _ <- ts += 100
        savepoint1 <- tdb.profile.unsafeCreateSavepoint("sp1")
        _ <- ts += 200
        _ <- ts.to[Set].result.map(_ shouldBe Set(100, 200))
        savepoint2 <- tdb.profile.unsafeCreateSavepoint("sp2")
        _ <- ts += 300
        _ <- ts.to[Set].result.map(_ shouldBe Set(100, 200, 300))
        _ <- tdb.profile.unsafeRollbackToSavepoint(savepoint2) // rollback to sp2
        _ <- ts.to[Set].result.map(_ shouldBe Set(100, 200)) // 300 should be gone
        _ <- ts += 400
        _ <- ts.to[Set].result.map(_ shouldBe Set(100, 200, 400))
        _ <- tdb.profile.unsafeRollbackToSavepoint(savepoint1) // rollback to sp1
        _ <- ts.result.map(_ shouldBe Seq(100)) // only 100 should remain
        _ <- tdb.profile.unsafeCommitTransaction
      } yield ()
    } andThen {
      ts.result.map(_ shouldBe Seq(100)) // transaction committed
    } andThen { // test savepoints with release
      for {
        _ <- tdb.profile.unsafeBeginTransaction
        _ <- ts += 500
        savepoint3 <- tdb.profile.unsafeCreateSavepoint("sp3")
        _ <- ts += 600
        _ <- ifCap(jcap.savepointRelease) {
          tdb.profile.unsafeReleaseSavepoint(savepoint3) // release savepoint
        }
        _ <- ts.to[Set].result.map(_ shouldBe Set(100, 500, 600))
        _ <- tdb.profile.unsafeCommitTransaction
      } yield ()
    } andThen {
      ts.to[Set].result.map(_ shouldBe Set(100, 500, 600)) // all changes committed
    } andThen {
      ts.schema.drop
    }
  }

  def testUnsafeTransactionPrimitives = {
    class T(tag: Tag) extends Table[Int](tag, "t_unsafe") {
      def a = column[Int]("a", O.PrimaryKey)
      def * = a
    }
    val ts = TableQuery[T]

    ts.schema.create andThen { // test manual transaction control with commit
      for {
        _ <- tdb.profile.unsafeBeginTransaction
        _ <- tdb.profile.isInTransaction.map(_ shouldBe true)
        _ <- GetTransactionality.map(_._1 shouldBe 1) // should be in transaction
        _ <- ts += 10
        _ <- ts.result.map(_ shouldBe Seq(10))
        _ <- tdb.profile.unsafeCommitTransaction
        _ <- tdb.profile.isInTransaction.map(_ shouldBe false)
        _ <- GetTransactionality.map(_._1 shouldBe 0) // should be outside transaction
      } yield ()
    } andThen {
      ts.result.map(_ shouldBe Seq(10)) // data should be committed
    } andThen { // test manual transaction control with rollback
      for {
        _ <- tdb.profile.unsafeBeginTransaction
        _ <- tdb.profile.isInTransaction.map(_ shouldBe true)
        _ <- ts += 20
        _ <- ts.to[Set].result.map(_ shouldBe Set(10, 20))
        _ <- tdb.profile.unsafeRollbackTransaction
        _ <- tdb.profile.isInTransaction.map(_ shouldBe false)
      } yield ()
    } andThen {
      ts.result.map(_ shouldBe Seq(10)) // data should be rolled back
    } andThen { // test isInTransaction outside transaction
      tdb.profile.isInTransaction.map(_ shouldBe false)
    } andThen {
      ts.schema.drop
    }
  }

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

package org.scalaquery.test

import org.junit.Test
import org.junit.Assert._
import org.scalaquery.session._
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object StatementParametersTest extends DBTestObject(H2Mem)

class StatementParametersTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  @Test def testExplicit() {
    println("*** Explicit ***")
    db withSession { s1:Session =>
      pr("start")(s1)
      ResultSetType.ScrollInsensitive(s1) { s2 =>
        pr("in ScrollInsensitive block")(s2)
        ResultSetHoldability.HoldCursorsOverCommit(s2) { s3 =>
          pr("in HoldCursorsOverCommit block")(s3)
          check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)(s1)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)(s2)
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit)(s3)
        }
        pr("back out")(s2)
      }
      pr("back out")(s1)
    }
  }

  @Test def testImplicit() {
    println("*** Implicit ***")
    import Database.threadLocalSession
    db withSession {
      pr("start")
      check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
      ResultSetType.ScrollInsensitive {
        pr("in ScrollInsensitive block")
        check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
        ResultSetHoldability.HoldCursorsOverCommit {
          pr("in HoldCursorsOverCommit block")
          check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.HoldCursorsOverCommit)
        }
        pr("back out")
        check(ResultSetType.ScrollInsensitive, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
      }
      pr("back out")
      check(ResultSetType.Auto, ResultSetConcurrency.Auto, ResultSetHoldability.Auto)
    }
  }

  def pr(msg: String)(implicit s: Session) = println(msg + ": " + s.resultSetType + " " + s.resultSetConcurrency + " " + s.resultSetHoldability)

  def check(t: ResultSetType, c: ResultSetConcurrency, h: ResultSetHoldability)(implicit s: Session) {
    assertEquals(s.resultSetType, t)
    assertEquals(s.resultSetConcurrency, c)
    assertEquals(s.resultSetHoldability, h)
  }
}

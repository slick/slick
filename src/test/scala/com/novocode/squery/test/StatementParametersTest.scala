package com.novocode.squery.test

import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.session._

object StatementParametersTest {
  def main(args: Array[String]) {
    new StatementParametersTest().testExplicit()
    new StatementParametersTest().testImplicit()
  }
}

class StatementParametersTest {
  @Test def testExplicit() {
    println("*** Explicit ***")
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession { s1 =>
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
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
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

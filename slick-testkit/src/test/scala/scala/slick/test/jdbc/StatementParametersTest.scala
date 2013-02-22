package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil._
import scala.slick.testutil.TestDBs._
import com.typesafe.slick.testkit.util.JdbcTestDB
import scala.slick.jdbc.{ResultSetType, ResultSetHoldability, ResultSetConcurrency}

object StatementParametersTest extends DBTestObject(H2Mem)

class StatementParametersTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.simple._

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

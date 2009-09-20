package test

import com.novocode.squery.session._

object StatementParametersTest {
  def main(args: Array[String]) {
    testExplicit()
    testImplicit()
  }

  def testExplicit() {
    println("*** Explicit ***")
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession { s1 =>
      pr("start", s1)
      ResultSetType.ScrollInsensitive(s1) { s2 =>
        pr("in ScrollInsensitive block", s2)
        ResultSetHoldability.HoldCursorsOverCommit(s2) { s3 =>
          pr("in HoldCursorsOverCommit block", s3)
        }
        pr("back out", s2)
      }
      pr("back out", s1)
    }
  }

  def testImplicit() {
    println("*** Implicit ***")
    import Database.threadLocalSession
    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
      pri("start")
      ResultSetType.ScrollInsensitive {
        pri("in ScrollInsensitive block")
        ResultSetHoldability.HoldCursorsOverCommit {
          pri("in HoldCursorsOverCommit block")
        }
        pri("back out")
      }
      pri("back out")
    }
  }

  def pr(msg: String, s: Session) = println(msg + ": " + s.resultSetType + " " + s.resultSetConcurrency + " " + s.resultSetHoldability)

  def pri(msg: String)(implicit s: Session) = pr(msg, s)
}

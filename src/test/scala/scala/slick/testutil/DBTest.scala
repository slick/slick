package scala.slick.testutil

import scala.collection.JavaConversions
import org.junit.Assert._
import org.junit.{Before, After}
import org.junit.runner.{JUnitCore, RunWith}
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

@RunWith(classOf[Parameterized])
abstract class DBTest {
  val tdb: TestDB

  println("[Using test database "+tdb+"]")
  lazy val db = tdb.createDB()
  private[this] var sessionCreated = false
  lazy val session = { sessionCreated = true; db.createSession() }
  @Before def beforeDBTest = tdb.cleanUpBefore()
  @After def afterDBTest = {
    try { if(sessionCreated) session.close() }
    finally { tdb.cleanUpAfter() }
  }

  def assertFail(f: =>Unit) = {
    var succeeded = false
    try { f; succeeded = true } catch { case _ => }
    if(succeeded) fail("Exception expected")
  }

  def assertAllMatch[T](t: TraversableOnce[T])(f: PartialFunction[T, _]) = t.foreach { x =>
    if(!f.isDefinedAt(x)) fail("Expected shape not matched by: "+x)
  }
}

abstract class DBTestObject(dbs: TestDB.TestDBSpec*) {
  val testClassName = {
    val s = getClass.getName
    s.substring(0, s.length-1)
  }
  def main(args: Array[String]) = JUnitCore.main(testClassName)
  @Parameters def parameters = JavaConversions.seqAsJavaList(dbs.map(n => n(this)).filter(_.isEnabled).map(to => Array(to)))
}

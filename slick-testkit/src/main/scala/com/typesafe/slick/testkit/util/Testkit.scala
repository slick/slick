package com.typesafe.slick.testkit.util

import scala.language.existentials
import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import org.junit.runners.model._
import org.junit.Assert._
import scala.slick.profile.{SqlProfile, Capability}
import scala.slick.driver.JdbcProfile
import com.typesafe.slick.testkit.{tests => tk}
import java.lang.reflect.Method

/** Lists all tests of the Slick driver test kit */
object Testkit {
  val tests: List[Class[_ <: TestkitTest]] =
    classOf[tk.AggregateTest] ::
    classOf[tk.ColumnDefaultTest] ::
    classOf[tk.CountTest] ::
    classOf[tk.DataTypeTest] ::
    classOf[tk.ExecutorTest] ::
    classOf[tk.ForeignKeyTest] ::
    classOf[tk.InsertTest] ::
    classOf[tk.InvokerTest] ::
    classOf[tk.IterateeTest] ::
    classOf[tk.JoinTest] ::
    classOf[tk.MainTest] ::
    classOf[tk.MapperTest] ::
    classOf[tk.MiscTest] ::
    classOf[tk.MutateTest] ::
    classOf[tk.NestingTest] ::
    classOf[tk.NewQuerySemanticsTest] ::
    classOf[tk.PagingTest] ::
    classOf[tk.PlainSQLTest] ::
    classOf[tk.PrimaryKeyTest] ::
    classOf[tk.ScalarFunctionTest] ::
    classOf[tk.SequenceTest] ::
    classOf[tk.TemplateTest] ::
    classOf[tk.TransactionTest] ::
    classOf[tk.UnionTest] ::
    (Nil: List[Class[_ <: TestkitTest]])
}

/** JUnit runner for the Slick driver test kit. */
class Testkit(clazz: Class[_ <: DriverTest], runnerBuilder: RunnerBuilder) extends SimpleParentRunner[TestMethod](clazz) {

  val driverTest = clazz.newInstance
  var tdb: TestDB = driverTest.tdbSpec(clazz.getName)

  def describeChild(ch: TestMethod) = ch.desc

  def getChildren = if(tdb.isEnabled) {
    driverTest.tests.flatMap { t =>
      val ms = t.getMethods.filter { m =>
        m.getName.startsWith("test") && m.getParameterTypes.length == 0
      }
      ms.map { m =>
        val tname = m.getName + '[' + tdb.confName + ']'
        new TestMethod(tname, Description.createTestDescription(t, tname), m, t)
      }
    }
  } else Nil

  override def runChildren(notifier: RunNotifier) = if(!children.isEmpty) {
    tdb.cleanUpBefore()
    try {
      val is = children.iterator.zipWithIndex.toIndexedSeq
      val last = is.length - 1
      var previousTestObject: TestkitTest = null
      for((ch, idx) <- is) {
        val desc = describeChild(ch)
        notifier.fireTestStarted(desc)
        try {
          val testObject =
            if(previousTestObject ne null) previousTestObject
            else ch.cl.getConstructor(classOf[TestDB]).newInstance(tdb)
          previousTestObject = null
          try {
            ch.method.invoke(testObject)
          } finally {
            val skipCleanup = idx == last || (testObject.reuseInstance && (ch.cl eq is(idx+1)._1.cl))
            if(skipCleanup) {
              if(idx == last) testObject.closeKeepAlive()
              else previousTestObject = testObject
            }
            else testObject.cleanup()
          }
        } catch {
          case t: Throwable => addFailure(t, notifier, desc)
        } finally notifier.fireTestFinished(desc)
      }
    } finally tdb.cleanUpAfter()
  }
}

abstract class DriverTest(val tdbSpec: TestDB.TestDBSpec) {
  def tests = Testkit.tests
}

case class TestMethod(name: String, desc: Description, method: Method, cl: Class[_ <: TestkitTest])

trait TestkitTest {
  val tdb: TestDB
  private[this] var keepAliveSession: tdb.profile.Backend#Session = null

  protected implicit def sharedSession: tdb.profile.Backend#Session = {
    db
    keepAliveSession
  }

  val reuseInstance = false

  lazy val db = {
    val db = tdb.createDB()
    keepAliveSession = db.createSession()
    if(!tdb.isPersistent && tdb.isShared)
      keepAliveSession.conn // keep the database in memory with an extra connection
    db
  }

  def cleanup() = if(keepAliveSession ne null) {
    try if(tdb.isPersistent) tdb.dropUserArtifacts(keepAliveSession)
    finally closeKeepAlive()
  }

  def closeKeepAlive() = {
    if(keepAliveSession ne null) keepAliveSession.close()
  }

  def assertFail(f: =>Unit) = {
    var succeeded = false
    try {
      f
      succeeded = true
    } catch {
      case e: Exception if !scala.util.control.Exception.shouldRethrow(e) =>
    }
    if(succeeded) fail("Exception expected")
  }

  def assertAllMatch[T](t: TraversableOnce[T])(f: PartialFunction[T, _]) = t.foreach { x =>
    if(!f.isDefinedAt(x)) fail("Expected shape not matched by: "+x)
  }

  def scap = SqlProfile.capabilities
  def jcap = JdbcProfile.capabilities
  def ifCap[T](caps: Capability*)(f: => T): Unit =
    if(caps.forall(c => tdb.capabilities.contains(c))) f
  def ifNotCap[T](caps: Capability*)(f: => T): Unit =
    if(!caps.forall(c => tdb.capabilities.contains(c))) f
}

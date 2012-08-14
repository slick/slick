package com.typesafe.slick.testkit.util

import scala.language.existentials
import org.junit.runner.Description
import org.junit.runner.notification.{Failure, RunNotifier}
import org.junit.runners.ParentRunner
import org.junit.runners.model.{Statement, RunnerBuilder}
import scala.slick.session.Session
import com.typesafe.slick.testkit.{tests => tk}
import scala.slick.testutil.TestDB
import org.junit.internal.runners.model.MultipleFailureException
import scala.collection.JavaConverters._
import java.lang.reflect.{InvocationTargetException, Method}
import org.junit.Assert._

/** JUnit runner for the Slick driver test kit. */
class Testkit(clazz: Class[_ <: DriverTest], runnerBuilder: RunnerBuilder) extends ParentRunner[TestMethod](clazz) {

  val tests: Seq[Class[_ <: TestkitTest]] = Seq(
    classOf[tk.AggregateTest],
    classOf[tk.BlobTest],
    classOf[tk.ColumnDefaultTest],
    classOf[tk.CountTest],
    classOf[tk.DataTypeTest],
    classOf[tk.ExecutorTest],
    classOf[tk.ForeignKeyTest],
    classOf[tk.InsertTest],
    classOf[tk.InvokerTest],
    classOf[tk.IterateeTest],
    classOf[tk.JoinTest],
    classOf[tk.MainTest],
    classOf[tk.MapperTest],
    classOf[tk.MiscTest],
    classOf[tk.MutateTest],
    classOf[tk.NestingTest],
    classOf[tk.NewQuerySemanticsTest],
    classOf[tk.OldTest],
    classOf[tk.PagingTest],
    classOf[tk.PrimaryKeyTest],
    classOf[tk.ScalarFunctionTest],
    classOf[tk.SequenceTest],
    classOf[tk.TemplateTest],
    classOf[tk.TransactionTest],
    classOf[tk.UnionTest],
    classOf[tk.ZipTest]
  )

  val spec = clazz.newInstance().tdbSpec
  var tdb: TestDB = spec(clazz.getName)

  def describeChild(ch: TestMethod) = ch.desc

  def getChildren: java.util.List[TestMethod] = if(tdb.isEnabled) {
    tests.flatMap { t =>
      val ms = t.getMethods.filter { m =>
        m.getName.startsWith("test") && m.getParameterTypes.length == 0
      }
      ms.map { m =>
        val tname = m.getName + '[' + tdb.confName + ']'
        new TestMethod(tname, Description.createTestDescription(t, tname), m, t)
      }
    }.toList.asJava
  } else new java.util.ArrayList[TestMethod]

  def runChild(ch: TestMethod, notifier: RunNotifier): Unit = {
    notifier.fireTestStarted(ch.desc)
    def addFailure(t: Throwable): Unit = t match {
      case t: MultipleFailureException =>
        t.getFailures.asScala.foreach(addFailure)
      case i: InvocationTargetException =>
        addFailure(i.getTargetException)
      case t: Throwable =>
        notifier.fireTestFailure(new Failure(ch.desc, t))
    }
    try {
      val cons = ch.cl.getConstructor(classOf[TestDB])
      val testObject = cons.newInstance(tdb)
      try ch.method.invoke(testObject) finally testObject.cleanup()
    } catch {
      case t: Throwable => addFailure(t)
    } finally notifier.fireTestFinished(ch.desc)
  }

  override def classBlock(notifier: RunNotifier) = {
    val s = super.classBlock(notifier)
    new Statement { def evaluate() {
      tdb.cleanUpBefore()
      try s.evaluate() finally tdb.cleanUpAfter()
    }}
  }
}

abstract class DriverTest(val tdbSpec: TestDB.TestDBSpec)

case class TestMethod(name: String, desc: Description, method: Method, cl: Class[_ <: TestkitTest])

abstract class TestkitTest {
  val tdb: TestDB
  private[this] var keepAliveSession: Session = null
  protected val useKeepAlive = true

  lazy val db = {
    val db = tdb.createDB()
    keepAliveSession = db.createSession()
    if(useKeepAlive && !tdb.isPersistent && tdb.isShared)
      keepAliveSession.conn // keep the database in memory with an extra connection
    db
  }

  def cleanup() = if(keepAliveSession ne null) {
    try if(tdb.isPersistent) tdb.dropUserArtifacts(keepAliveSession)
    finally keepAliveSession.close()
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

  def cap = tdb.driver.capabilities
}

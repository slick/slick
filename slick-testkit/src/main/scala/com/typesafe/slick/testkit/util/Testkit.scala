package com.typesafe.slick.testkit.util

import scala.language.existentials

import scala.concurrent.{ExecutionContext, Await, Future}
import scala.reflect.ClassTag

import java.lang.reflect.Method
import java.util.concurrent.{ExecutionException, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import scala.slick.action._
import scala.slick.util.DumpInfo
import scala.util.control.NonFatal
import scala.slick.profile.{RelationalProfile, SqlProfile, Capability}
import scala.slick.driver.JdbcProfile

import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import org.junit.runners.model._
import org.junit.Assert

/** JUnit runner for the Slick driver test kit. */
class Testkit(clazz: Class[_ <: DriverTest], runnerBuilder: RunnerBuilder) extends SimpleParentRunner[TestMethod](clazz) {

  val driverTest = clazz.newInstance
  var tdb: TestDB = driverTest.tdb

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
      val is = children.iterator.map(ch => (ch, ch.cl.newInstance()))
        .filter{ case (_, to) => to.setTestDB(tdb) }.zipWithIndex.toIndexedSeq
      val last = is.length - 1
      var previousTestObject: GenericTest[_ >: Null <: TestDB] = null
      for(((ch, preparedTestObject), idx) <- is) {
        val desc = describeChild(ch)
        notifier.fireTestStarted(desc)
        try {
          val testObject =
            if(previousTestObject ne null) previousTestObject
            else preparedTestObject
          previousTestObject = null
          try ch.run(testObject) finally {
            val skipCleanup = idx == last || (testObject.reuseInstance && (ch.cl eq is(idx+1)._1._1.cl))
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

abstract class DriverTest(val tdb: TestDB) {
  def tests = TestkitConfig.testClasses
}

case class TestMethod(name: String, desc: Description, method: Method, cl: Class[_ <: GenericTest[_ >: Null <: TestDB]]) {
  private[this] def await[T](f: Future[T]): T =
    try Await.result(f, TestkitConfig.asyncTimeout)
    catch { case ex: ExecutionException => throw ex.getCause }

  def run(testObject: GenericTest[_]): Unit = {
    val r = method.getReturnType
    testObject match {
      case testObject: TestkitTest[_] =>
        if(r == Void.TYPE) method.invoke(testObject)
        else throw new RuntimeException(s"Illegal return type: '${r.getName}' in test method '$name' -- TestkitTest methods must return Unit")

      case testObject: AsyncTest[_] =>
        if(r == classOf[Future[_]]) await(method.invoke(testObject).asInstanceOf[Future[Any]])
        else if(r == classOf[Action[_, _]]) await(testObject.db.run(method.invoke(testObject).asInstanceOf[Action[Effect, Any]]))
        else throw new RuntimeException(s"Illegal return type: '${r.getName}' in test method '$name' -- AsyncTest methods must return Future or Action")
    }
  }
}

sealed abstract class GenericTest[TDB >: Null <: TestDB](implicit TdbClass: ClassTag[TDB]) {
  protected[this] var _tdb: TDB = null
  private[testkit] def setTestDB(tdb: TestDB): Boolean = {
    tdb match {
      case TdbClass(o) =>
        _tdb = o
        true
      case _ =>
        false
    }
  }
  lazy val tdb: TDB = _tdb

  private[testkit] var keepAliveSession: tdb.profile.Backend#Session = null

  private[this] var unique = new AtomicInteger

  val reuseInstance = false

  lazy val db = {
    val db = tdb.createDB()
    keepAliveSession = db.createSession()
    if(!tdb.isPersistent && tdb.isShared)
      keepAliveSession.force() // keep the database in memory with an extra connection
    db
  }

  final def cleanup() = if(keepAliveSession ne null) {
    try if(tdb.isPersistent) tdb.dropUserArtifacts(keepAliveSession)
    finally {
      try db.close() finally closeKeepAlive()
    }
  }

  final def closeKeepAlive() = {
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
    if(succeeded) Assert.fail("Exception expected")
  }

  def assertAllMatch[T](t: TraversableOnce[T])(f: PartialFunction[T, _]) = t.foreach { x =>
    if(!f.isDefinedAt(x)) Assert.fail("Expected shape not matched by: "+x)
  }

  implicit class AssertionExtensionMethods(v: Any) {
    private[this] val cln = getClass.getName
    private[this] def fixStack(f: => Unit): Unit = try f catch {
      case ex: AssertionError =>
        ex.setStackTrace(ex.getStackTrace.iterator.filterNot(_.getClassName.startsWith(cln)).toArray)
        throw ex
    }

    def shouldBe(o: Any): Unit = fixStack(Assert.assertEquals(o, v))

    def shouldNotBe(o: Any): Unit = fixStack(Assert.assertNotSame(o, v))
  }

  implicit class StringContextExtensionMethods(s: StringContext) {
    /** Generate a unique name suitable for a database entity */
    def u(args: Any*) = s.standardInterpolator(identity, args) + "_" + unique.incrementAndGet()
  }

  def rcap = RelationalProfile.capabilities
  def scap = SqlProfile.capabilities
  def jcap = JdbcProfile.capabilities
  def tcap = TestDB.capabilities
  def ifCap[T](caps: Capability*)(f: => T): Unit =
    if(caps.forall(c => tdb.capabilities.contains(c))) f
  def ifNotCap[T](caps: Capability*)(f: => T): Unit =
    if(!caps.forall(c => tdb.capabilities.contains(c))) f
}

abstract class TestkitTest[TDB >: Null <: TestDB](implicit TdbClass: ClassTag[TDB]) extends GenericTest[TDB] {
  @deprecated("Use implicitSession instead of sharedSession", "2.2")
  protected final def sharedSession: tdb.profile.Backend#Session = implicitSession

  protected implicit def implicitSession: tdb.profile.Backend#Session = {
    db
    keepAliveSession
  }
}

abstract class AsyncTest[TDB >: Null <: TestDB](implicit TdbClass: ClassTag[TDB]) extends GenericTest[TDB] {
  final override val reuseInstance = true

  protected implicit def asyncTestExecutionContext = ExecutionContext.global

  object GetSession extends SynchronousDatabaseAction[TDB#Driver#Backend, Effect, TDB#Driver#Backend#Session] {
    def run(context: ActionContext[TDB#Driver#Backend]) = context.session
    def getDumpInfo = DumpInfo(name = "GetSession")
  }

  object IsPinned extends SynchronousDatabaseAction[TDB#Driver#Backend, Effect, Boolean] {
    def run(context: ActionContext[TDB#Driver#Backend]) = context.isPinned
    def getDumpInfo = DumpInfo(name = "IsPinned")
  }
}

package com.typesafe.slick.testkit.util

import java.lang.reflect.Method
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream

import slick.SlickTreeException
import slick.basic.Capability
import slick.dbio.*
import slick.jdbc.{JdbcBackend, JdbcCapabilities}
import slick.lifted.Rep
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities
import slick.util.DumpInfo

import org.junit.runner.Description
import org.junit.runner.notification.RunNotifier
import org.junit.runners.model.*
import org.junit.Assert
import org.slf4j.MDC


/** JUnit runner for the Slick driver test kit. */
class Testkit(clazz: Class[? <: ProfileTest], runnerBuilder: RunnerBuilder)
  extends SimpleParentRunner[TestMethod](clazz) {

  val profileTest: ProfileTest = clazz.getConstructor().newInstance()
  var tdb: TestDB = profileTest.tdb

  def describeChild(ch: TestMethod) = ch.desc

  def getChildren =
    profileTest.tests.flatMap { t =>
      val ms = t.getMethods.filter { m =>
        m.getName.startsWith("test") && m.getParameterTypes.length == 0
      }
      ms.map { m =>
        val typeName = m.getName + '[' + tdb.confName + ']'
        TestMethod(typeName, Description.createTestDescription(t, typeName), m, t)
      }
    }

  override def runChildren(notifier: RunNotifier) =
    if (!tdb.isEnabled)
      children.foreach(child => notifier.fireTestIgnored(describeChild(child)))
    else {
      tdb.cleanUpBefore()
      try {
        val is = (children.iterator.map(ch => (ch, ch.cl.getConstructor().newInstance().asInstanceOf[AsyncTest[? >: Null <: TestDB]]))) //TODO why does Dotty require this cast?
          .filter { case (_, to) => to.setTestDB(tdb) }.zipWithIndex.toIndexedSeq
        val last = is.length - 1
        var previousTestObject: AsyncTest[? >: Null <: TestDB] = null
        for (((ch, preparedTestObject), idx) <- is) {
          val desc = describeChild(ch)
          notifier.fireTestStarted(desc)
          try {
            val testObject =
              if (previousTestObject ne null) previousTestObject
              else preparedTestObject
            previousTestObject = null
            try ch.run(testObject) finally {
              val skipCleanup = idx == last || (testObject.reuseInstance && (ch.cl eq is(idx + 1)._1._1.cl))
              if (skipCleanup) {
                if (idx == last) testObject.closeKeepAlive()
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

abstract class ProfileTest(val tdb: TestDB) {
  def tests = tdb.testClasses
}

case class TestMethod(name: String, desc: Description, method: Method, cl: Class[? <: AsyncTest[? >: Null <: TestDB]]) {
  def run(testObject: AsyncTest[?]): Unit = {
    val r = method.getReturnType
    if (r == classOf[IO[?]])
      method.invoke(testObject).asInstanceOf[IO[Any]].unsafeRunSync()
    else if (r == classOf[DBIOAction[?, ?, ?]])
      testObject.db.run(method.invoke(testObject).asInstanceOf[DBIO[Any]]).unsafeRunSync()
    else
      throw new RuntimeException(
        s"Illegal return type: '${r.getName}' in test method '$name' -- AsyncTest methods must return IO or DBIO"
      )
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
  final lazy val tdb: TDB = _tdb

  private[testkit] var keepAliveSession: tdb.profile.backend.Session = null

  private[this] var unique = new AtomicInteger

  val reuseInstance = false

  lazy val db: tdb.IODatabase = {
    val d = tdb.createDB()
    keepAliveSession = d.createSession()
    if(!tdb.isPersistent && tdb.isShared)
      keepAliveSession.force() // keep the database in memory with an extra connection
    d
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

  implicit class StringExtensionMethods(s: String) {
    /** Generate a unique name suitable for a database entity */
    def withUniquePostFix: String = {
      s"${s}_${unique.incrementAndGet()}"
    }
  }

  final def mark[T](id: String, f: => T): T = {
    def set(id: String): Unit =
      if(id eq null) MDC.remove("debugId")
      else MDC.put("debugId", id)
    val old = MDC.get("debugId")
    try {
      set(if(id eq null) id else s" [$id]")
      f
    } finally set(old)
  }

  final def mark[R, S <: NoStream, E <: Effect](id: String, f: => DBIOAction[R, S, E]): DBIOAction[R, S, E] =
    mark[DBIOAction[R, S, E]](id, f.named(id))

  def assertNesting(q: Rep[?], exp: Int): Unit = {
    import slick.ast.*
    import slick.ast.Util.*
    import slick.compiler.QueryCompiler
    val qc = new QueryCompiler(tdb.profile.queryCompiler.phases.takeWhile(_.name != "codeGen"))
    val cs = qc.run(q.toNode)
    val found = cs.tree.collect { case c: Comprehension.Base => c }.length
    if(found != exp)
      throw cs.symbolNamer.use(new SlickTreeException(s"Found $found Comprehension nodes, should be $exp",
        cs.tree, mark = _.isInstanceOf[Comprehension.Base], removeUnmarked = false))
  }

  def rcap = RelationalCapabilities
  def scap = SqlCapabilities
  def jcap = JdbcCapabilities
  def tcap = TestDB.capabilities
}

abstract class AsyncTest[TDB >: Null <: TestDB](implicit TdbClass: ClassTag[TDB]) extends GenericTest[TDB] {
  final override val reuseInstance = true

  /** Test Action: Get the current database session */
  object GetSession
    extends SynchronousDatabaseAction[tdb.profile.backend.Session, NoStream, tdb.profile.backend.Context, tdb.profile.backend.StreamingContext, Effect] {
    def run(context: tdb.profile.backend.Context) = context.session
    def getDumpInfo = DumpInfo(name = "<GetSession>")
  }

  /** Test Action: Check if the current database session is pinned */
  object IsPinned extends SynchronousDatabaseAction[Boolean, NoStream, tdb.profile.backend.Context, tdb.profile.backend.StreamingContext, Effect] {
    def run(context: tdb.profile.backend.Context) = context.isPinned
    def getDumpInfo = DumpInfo(name = "<IsPinned>")
  }

  /** Test Action: Get the current transactionality level and autoCommit flag.
    * Returns (transactionDepth, autoCommit) where autoCommit = (transactionDepth == 0).
    * In Slick 4, nesting depth is tracked in ExecState.transactionDepth;
    * the old BaseSession.getTransactionality no longer exists. */
  object GetTransactionality extends SynchronousDatabaseAction[(Int, Boolean), NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
    def run(context: JdbcBackend#JdbcActionContext) = {
      val depth = context.transactionDepth
      (depth, depth == 0)
    }
    def getDumpInfo = DumpInfo(name = "<GetTransactionality>")
  }

  /** Test Action: Get the current statement parameters, except for `statementInit` which is always set to null */
  object GetStatementParameters
    extends SynchronousDatabaseAction[JdbcBackend.StatementParameters, NoStream, JdbcBackend#JdbcActionContext, JdbcBackend#JdbcStreamingActionContext, Effect] {
    def run(context: JdbcBackend#JdbcActionContext) = {
      val s = context.session
      JdbcBackend.StatementParameters(
        s.resultSetType,
        s.resultSetConcurrency,
        s.resultSetHoldability,
        null,
        s.fetchSize
      )
    }
    def getDumpInfo = DumpInfo(name = "<GetStatementParameters>")
  }

  def ifCap[E <: Effect, R](caps: Capability*)(f: => DBIOAction[R, NoStream, E]): DBIOAction[Unit, NoStream, E] =
    if(caps.forall(c => tdb.capabilities.contains(c))) f.andThen(DBIO.successful(())) else DBIO.successful(())
  def ifNotCap[E <: Effect, R](caps: Capability*)(f: => DBIOAction[R, NoStream, E]): DBIOAction[Unit, NoStream, E] =
    if(!caps.forall(c => tdb.capabilities.contains(c))) f.andThen(DBIO.successful(())) else DBIO.successful(())

  def ifCapF[R](caps: Capability*)(f: => IO[R]): IO[Unit] =
    if(caps.forall(c => tdb.capabilities.contains(c))) f.map(_ => ()) else IO.unit
  def ifNotCapF[R](caps: Capability*)(f: => IO[R]): IO[Unit] =
    if(!caps.forall(c => tdb.capabilities.contains(c))) f.map(_ => ()) else IO.unit

  def ifCapU[T](caps: Capability*)(f: => T): Unit =
    if(caps.forall(c => tdb.capabilities.contains(c))) f
  def ifNotCapU[T](caps: Capability*)(f: => T): Unit =
    if(!caps.forall(c => tdb.capabilities.contains(c))) f

  def seq[E <: Effect](actions: DBIOAction[?, NoStream, E]*): DBIOAction[Unit, NoStream, E] = DBIO.seq[E](actions: _*)

  /** Consume a Stream and materialize it as a Vector. */
  def materialize[T](p: Stream[IO, T]): IO[Vector[T]] =
    p.compile.toVector

  /** Iterate over a Stream. */
  def foreach[T](p: Stream[IO, T])(f: T => Any): IO[Unit] =
    p.evalMap(t => IO(f(t))).compile.drain

  /** Asynchronously consume a Stream and materialize it as a Vector, processing elements
    * one-by-one after a delay. This ensures transformation doesn't run in the synchronous
    * database context but still preserves proper sequencing. */
  def materializeAsync[T, R](p: Stream[IO, T],
                             tr: T => IO[R],
                             delay: Duration = Duration(100L, TimeUnit.MILLISECONDS)): IO[Vector[R]] = {
    p.evalMap { t =>
      IO.sleep(scala.concurrent.duration.FiniteDuration(delay.toMillis, TimeUnit.MILLISECONDS)) >>
      tr(t)
    }.compile.toVector
  }

  implicit class AssertionExtensionMethods[T](v: T) {
    private[this] val cln = getClass.getName
    private[this] def fixStack(f: => Unit): Unit = try f catch {
      case ex: AssertionError =>
        ex.setStackTrace(ex.getStackTrace.iterator.filterNot(_.getClassName.startsWith(cln)).toArray)
        throw ex
    }

    def shouldBe(o: Any): Unit = fixStack(Assert.assertEquals(o, v))

    def shouldNotBe(o: Any): Unit = fixStack(Assert.assertNotSame(o, v))

    def should(f: T => Boolean): Unit = fixStack(Assert.assertTrue("'should' assertion failed for value: "+v, f(v)))

    def shouldFail(f: T => Unit): Unit = {
      var ok = false
      try { f(v); ok = true } catch { case _: Throwable => }
      if(ok) fixStack(Assert.fail("Expected failure"))
    }

    def shouldBeA(implicit ct: ClassTag[T]): Unit = {
      if(!ct.runtimeClass.isInstance(v))
        fixStack(Assert.fail("Expected value of type " + ct.runtimeClass.getName + ", got " + v.getClass.getName))
    }
  }

  implicit class CollectionAssertionExtensionMethods[T](v: IterableOnce[T]) {
    private[this] val cln = getClass.getName
    private[this] def fixStack(f: => Unit): Unit = try f catch {
      case ex: AssertionError =>
        ex.setStackTrace(ex.getStackTrace.iterator.filterNot(_.getClassName.startsWith(cln)).toArray)
        throw ex
    }

    def shouldAllMatch(f: PartialFunction[T, ?]) = v.iterator.foreach { x =>
      if(!f.isDefinedAt(x)) fixStack(Assert.fail("Value does not match expected shape: "+x))
    }
  }

  implicit class DBIOActionExtensionMethods[T, +S <: NoStream, -E <: Effect](action: DBIOAction[T, S, E]) {
    def shouldYield(t: T) = action.map(_.shouldBe(t))
  }

  implicit class CollectionDBIOActionExtensionMethods[T, +S <: NoStream, -E <: Effect](action:
                                                                                       DBIOAction[Vector[T], S, E]) {
    def shouldYield(t: Set[T]) = action.map(_.toSet.shouldBe(t))
    def shouldYield(t: Seq[T]) = action.map(_.shouldBe(t))
    def shouldYield(t: List[T]) = action.map(_.toList.shouldBe(t))
  }
}

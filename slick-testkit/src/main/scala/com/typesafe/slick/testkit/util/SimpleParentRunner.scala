package com.typesafe.slick.testkit.util

import org.junit.runner.{Runner, Description}
import org.junit.runner.notification.{StoppedByUserException, Failure, RunNotifier}
import org.junit.runner.manipulation._
import org.junit.runners.model._
import scala.collection.JavaConverters._
import java.lang.reflect.InvocationTargetException

/**
 * A JUnit Runner similar to JUnit's own ParentRunner but simpler, more
 * extensible (in the way we need it), and more Scala-like.
 */
abstract class SimpleParentRunner[T](testClass: Class[_]) extends Runner with Filterable with Sortable {

  private var _children: Seq[T] = null
  protected final def children = {
    if(_children == null) _children = getChildren
    _children
  }
  protected final def children_= (s: Seq[T]) = _children = s

  protected def getChildren: Seq[T]

  protected def describeChild(child: T): Description

  protected def runChildInner(child: T, notifier: RunNotifier) = ???

  protected def runChild(child: T, notifier: RunNotifier): Unit = {
    val desc = describeChild(child)
    notifier.fireTestStarted(desc)
    try runChildInner(child, notifier) catch {
      case t: Throwable => addFailure(t, notifier, desc)
    } finally notifier.fireTestFinished(desc)
  }

  protected def runChildren(notifier: RunNotifier) =
    children.foreach(ch => runChild(ch, notifier))

  protected final def addFailure(t: Throwable, notifier: RunNotifier, desc: Description): Unit = t match {
    case t: MultipleFailureException =>
      t.getFailures.asScala.foreach(t2 => addFailure(t2, notifier, desc))
    case i: InvocationTargetException =>
      addFailure(i.getTargetException, notifier, desc)
    case t: Throwable =>
      notifier.fireTestFailure(new Failure(desc, t))
  }

  def getDescription = {
    val desc = Description.createSuiteDescription(testClass.getName, testClass.getAnnotations: _*)
    for(ch <- children) desc.addChild(describeChild(ch))
    desc
  }

  def run(notifier: RunNotifier): Unit = {
    try runChildren(notifier) catch {
      case e: StoppedByUserException => throw e
      case e: Throwable => addFailure(e, notifier, getDescription)
    }
  }

  final def filter(filter: Filter): Unit = {
    children = children.filter { ch =>
      if(!filter.shouldRun(describeChild(ch))) false
      else try { filter.apply(ch); true } catch { case _: NoTestsRemainException => false }
    }
    if(children.isEmpty) throw new NoTestsRemainException
  }

  final def sort(sorter: Sorter): Unit = {
    children.foreach(sorter.apply _)
    children = children.sorted(new Ordering[T] {
      def compare(o1: T, o2: T): Int = sorter.compare(describeChild(o1), describeChild(o2))
    })
  }
}

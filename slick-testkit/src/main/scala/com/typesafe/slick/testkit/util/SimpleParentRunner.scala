package com.typesafe.slick.testkit.util

import java.lang.reflect.InvocationTargetException

import scala.jdk.CollectionConverters.*

import org.junit.runner.{Description, Runner}
import org.junit.runner.manipulation.{Ordering as _, *}
import org.junit.runner.notification.{Failure, RunNotifier, StoppedByUserException}
import org.junit.runners.model.*

/**
 * A JUnit Runner similar to JUnit's own ParentRunner but simpler, more
 * extensible (in the way we need it), and more Scala-like.
 */
abstract class SimpleParentRunner[T](testClass: Class[?]) extends Runner with Filterable with Sortable {
  private var _children: Seq[T] = _
  protected final def children: Seq[T] = {
    if(_children == null) _children = getChildren
    _children
  }
  protected final def children_= (s: Seq[T]) = _children = s

  protected def getChildren: Seq[T]

  protected def describeChild(child: T): Description

  protected def runChildren(notifier: RunNotifier): Unit

  protected final def addFailure(t: Throwable, notifier: RunNotifier, desc: Description): Unit = t match {
    case t: MultipleFailureException =>
      t.getFailures.asScala.foreach(t2 => addFailure(t2, notifier, desc))
    case i: InvocationTargetException =>
      addFailure(i.getTargetException, notifier, desc)
    case t: Throwable =>
      notifier.fireTestFailure(new Failure(desc, t))
  }

  def getDescription = {
    val annotations = testClass.getAnnotations
    val desc = Description.createSuiteDescription(testClass.getName, annotations *)
    for(ch <- children) desc.addChild(describeChild(ch))
    desc
  }

  def run(notifier: RunNotifier): Unit = {
    val description = getDescription
    notifier.fireTestSuiteStarted(description)
    try runChildren(notifier)
    catch {
      case e: StoppedByUserException => throw e
      case e: Throwable              => addFailure(e, notifier, description)
    }
    finally notifier.fireTestSuiteFinished(description)
  }

  final def filter(filter: Filter): Unit = {
    children = children.filter { ch =>
      if(!filter.shouldRun(describeChild(ch))) false
      else try { filter.apply(ch); true } catch { case _: NoTestsRemainException => false }
    }
    if(children.isEmpty) throw new NoTestsRemainException
  }

  final def sort(sorter: Sorter): Unit = {
    children.foreach(sorter.apply)
    children =
      children.sorted(
        Ordering.comparatorToOrdering(sorter)
          .on(describeChild)
      )
  }
}

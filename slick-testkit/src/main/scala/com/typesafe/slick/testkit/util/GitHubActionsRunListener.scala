package com.typesafe.slick.testkit.util

import java.nio.file.{Files, Paths}
import java.time.Duration

import scala.jdk.CollectionConverters.*

import org.junit.runner.{Description, Result}
import org.junit.runner.notification.{Failure, RunListener}


class GitHubActionsRunListener extends RunListener {
  private def error(title: String = null)(value: String): Unit = {
    val titlePart = Option(title).fold("")(" title=" + _)
    println(s"::error$titlePart::$value")
  }

  override def testFailure(failure: Failure) =
    error(failure.getTestHeader + " failed")(failure.getMessage)
  override def testAssumptionFailure(failure: Failure) =
    error(failure.getTestHeader + " failed an assumption")(failure.getMessage)

  override def testRunStarted(description: Description) =
    println(s"$this: ${description.getTestClass} started")

  override def testRunFinished(result: Result) = {
    val (runs, failures, ignores) =
      (result.getRunCount, result.getFailureCount + result.getAssumptionFailureCount, result.getIgnoreCount)

    if (runs == 0)
      error()(s"$failures tests failed and $ignores were ignored")

    val dur = Duration.ofMillis(result.getRunTime)
    val minutes = dur.toMinutes

    val summary = Seq(
      if (runs > 0) s":heavy_check_mark: $runs tests passed" else "",
      if (failures > 0) s":exclamation: $failures tests failed" else "",
      if (ignores > 0) s":zzz: $ignores tests were ignored" else "",
      s":watch: Completed in ${minutes}m ${dur.minusMinutes(minutes).toMillis / 1000.0}s"
    )

    sys.env.get("GITHUB_STEP_SUMMARY") match {
      case None           => println("SUMMARY\n" + summary.mkString("\n"))
      case Some(filename) => Files.write(Paths.get(filename), summary.asJava)
    }
  }
}

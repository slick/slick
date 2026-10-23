package com.typesafe.slick.docs.test

import java.io.{ByteArrayOutputStream, PrintStream}
import java.lang.reflect.{InvocationTargetException, Modifier}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

import org.junit.runner.{Description, RunWith, Runner}
import org.junit.runner.notification.{Failure, RunNotifier}

/** Runs every example in doc/code, the code that the manual includes with `@@snip`, each as its own test.
  *
  * Each top-level object in doc/code is an example. One with a `main` method is run by calling it; any other is only
  * initialized. Either way, an exception fails its test. When `slick-testkit/src/doctest/resources/<object>.check`
  * exists, what the example prints must equal it, except that lines starting with "select " are compared as
  * "select...". What each example prints is also written to `<object>-run.log` next to it. Output printed from
  * other threads (for example inside a Future) is not captured.
  *
  * Two more tests check the setup: every file in doc/code has an example, and [[DocExamplesTest.skipped]] names only
  * examples that exist.
  */
@RunWith(classOf[DocExamplesTest.ExamplesRunner])
class DocExamplesTest

object DocExamplesTest {

  /** Examples that cannot run here, each with the reason. They are reported as ignored. */
  val skipped: Map[String, String] = Map.empty

  private val examplesDir = Paths.get("doc/code")
  private val resourcesDir = Paths.get("slick-testkit/src/doctest/resources")

  class ExamplesRunner(testClass: Class[?]) extends Runner {
    private val examplesByFile = discoverExamples()
    private val examples = examplesByFile.values.flatten.toSeq.sorted

    private def test(name: String) = Description.createTestDescription(testClass, name)
    private val everyFileHasAnExample = test("every file in doc/code has an example")
    private val skipsExist = test("skipped names only existing examples")
    private val exampleTests = examples.map(example => example -> test(example))

    override val getDescription: Description = {
      val suite = Description.createSuiteDescription(testClass)
      (everyFileHasAnExample +: skipsExist +: exampleTests.map(_._2)).foreach(suite.addChild)
      suite
    }

    override def run(notifier: RunNotifier): Unit = {
      def report(description: Description)(failure: => Option[Throwable]): Unit = {
        notifier.fireTestStarted(description)
        failure.foreach(e => notifier.fireTestFailure(new Failure(description, e)))
        notifier.fireTestFinished(description)
      }

      report(everyFileHasAnExample) {
        val filesWithoutExamples = examplesByFile.collect { case (file, Nil) => file }
        if (filesWithoutExamples.isEmpty) None
        else Some(new AssertionError(s"No top-level object, so nothing in them is run: ${filesWithoutExamples.mkString(", ")}"))
      }
      report(skipsExist) {
        val unknown = skipped.keySet -- examples
        if (unknown.isEmpty) None else Some(new AssertionError(s"Not examples: ${unknown.mkString(", ")}"))
      }
      exampleTests.foreach { case (example, description) =>
        if (skipped.contains(example)) notifier.fireTestIgnored(description)
        else report(description)(runExample(example))
      }
    }
  }

  /** The top-level objects in each file in doc/code */
  private def discoverExamples(): Map[Path, Seq[String]] = {
    val files = Files.list(examplesDir)
    try
      files.iterator().asScala.filter(_.toString.endsWith(".scala")).map { file =>
        val source = new String(Files.readAllBytes(file), UTF_8)
        file -> """(?m)^object (\w+)""".r.findAllMatchIn(source).map(_.group(1)).toList
      }.toMap
    finally files.close()
  }

  /** Runs one example; returns why it failed, if it did */
  private def runExample(example: String): Option[Throwable] = {
    val printed = new ByteArrayOutputStream()
    val error =
      try {
        Console.withOut(new PrintStream(printed, true, "UTF-8"))(invoke(example))
        None
      } catch {
        case NonFatal(e) => Some(e)
      }

    val output = new String(printed.toByteArray, UTF_8).linesIterator.map(mask).map(_ + "\n").mkString
    print(output)
    Files.write(resourcesDir.resolve(s"$example-run.log"), output.getBytes(UTF_8))

    error.orElse {
      val checkFile = resourcesDir.resolve(s"$example.check")
      if (Files.exists(checkFile) && new String(Files.readAllBytes(checkFile), UTF_8) != output)
        Some(new AssertionError(s"Its output does not match $checkFile; see $example-run.log next to it"))
      else None
    }
  }

  private def invoke(example: String): Unit =
    try {
      val mainMethod =
        Class.forName(s"com.typesafe.slick.docs.$example").getMethods.find { m =>
          m.getName == "main" && Modifier.isStatic(m.getModifiers) &&
          m.getParameterTypes.sameElements(Array(classOf[Array[String]]))
        }
      mainMethod match {
        case Some(main) => main.invoke(null, Array.empty[String])
        case None       => Class.forName(s"com.typesafe.slick.docs.$example$$").getField("MODULE$").get(null)
      }
      ()
    } catch {
      case e: InvocationTargetException   => throw e.getCause
      case e: ExceptionInInitializerError => throw e.getCause
    }

  private def mask(line: String): String = if (line.startsWith("select ")) "select..." else line
}

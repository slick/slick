package com.typesafe.slick.docs.test

import java.io.{ByteArrayOutputStream, PrintStream}
import java.lang.reflect.Modifier
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

import com.typesafe.slick.testkit.util.SimpleParentRunner
import org.junit.Assert
import org.junit.runner.{Description, RunWith}
import org.junit.runner.notification.RunNotifier

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
  val skipped: Map[String, String] = Map(
    "ConnectingFuture" -> "Its \"mydb\" config points at a Postgres or MySQL server",
    "ConnectingZio" -> "Its \"mydb\" config points at a Postgres or MySQL server"
  )

  private val examplesDir = Paths.get("doc/code")
  private val resourcesDir = Paths.get("slick-testkit/src/doctest/resources")

  /** One test: an example, or one of the two checks of the setup */
  final case class Check(name: String, run: () => Unit)

  class ExamplesRunner(testClass: Class[?]) extends SimpleParentRunner[Check](testClass) {

    protected def getChildren: Seq[Check] = {
      val examplesByFile = discoverExamples()
      val examples = examplesByFile.values.flatten.toSeq.sorted
      Seq(
        Check("every file in doc/code has an example", { () =>
          val filesWithoutExamples = examplesByFile.collect { case (file, Nil) => file }
          Assert.assertTrue(
            s"No top-level object, so nothing in them is run: ${filesWithoutExamples.mkString(", ")}",
            filesWithoutExamples.isEmpty
          )
        }),
        Check("skipped names only existing examples", { () =>
          val unknown = skipped.keySet -- examples
          Assert.assertTrue(s"Not examples: ${unknown.mkString(", ")}", unknown.isEmpty)
        })
      ) ++ examples.map(example => Check(example, () => runExample(example)))
    }

    protected def describeChild(check: Check): Description = Description.createTestDescription(testClass, check.name)

    protected def runChildren(notifier: RunNotifier): Unit =
      children.foreach { check =>
        val description = describeChild(check)
        if (skipped.contains(check.name)) notifier.fireTestIgnored(description)
        else {
          notifier.fireTestStarted(description)
          try check.run()
          catch { case NonFatal(e) => addFailure(e, notifier, description) }
          finally notifier.fireTestFinished(description)
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

  private def runExample(example: String): Unit = {
    val printed = new ByteArrayOutputStream()
    def output = new String(printed.toByteArray, UTF_8).linesIterator.map(mask).map(_ + "\n").mkString
    try Console.withOut(new PrintStream(printed, true, "UTF-8"))(invoke(example))
    finally {
      print(output)
      Files.write(resourcesDir.resolve(s"$example-run.log"), output.getBytes(UTF_8))
    }

    val checkFile = resourcesDir.resolve(s"$example.check")
    if (Files.exists(checkFile))
      Assert.assertEquals(s"Output does not match $checkFile", new String(Files.readAllBytes(checkFile), UTF_8), output)
  }

  /** Calls the example's main, or initializes the object if it has none. `SimpleParentRunner` unwraps the
    * InvocationTargetException a failing main throws; an exception in the object's initializer is unwrapped here. */
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
      case e: ExceptionInInitializerError => throw e.getCause
    }

  private def mask(line: String): String = if (line.startsWith("select ")) "select..." else line
}

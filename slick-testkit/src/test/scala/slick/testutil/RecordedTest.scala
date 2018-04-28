package slick.testutil

import org.junit.{Test, Assert}
import java.io._
import scala.Console

/**
 * A JUnit test which compares the console output against a recorded test run.
 */
abstract class RecordedTest {
  private[this] var _out: PrintStream = _

  def out = _out

  def run: Unit

  def basename: String

  def mask(line: String): String = line

  @Test def test(): Unit = {
    val logData = {
      val log = new ByteArrayOutputStream()
      val logP = new PrintStream(log)
      _out = logP
      Console.withOut(logP)(run)
      val logA = log.toByteArray
      Console.out.write(logA)
      val r = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(logA)))
      val masked = new StringWriter()
      val w = new BufferedWriter(masked)
      var line: String = null
      while({ line = r.readLine(); line != null }) w.write(mask(line)+"\n")
      w.flush()
      masked.getBuffer.toString
    }

    val check = {
      val f = new File(basename+".check")
      if(f.isFile) Some(read(f))
      else {
        // Disabled because now we have doctests without a check file:
        // Console.err.println("Warning: "+f+" not found")
        None
      }
    }

    val out = new FileOutputStream(new File(basename+"-run.log"))
    try {
      val w = new OutputStreamWriter(out, "UTF-8")
      w.write(logData)
      w.close
    } finally out.close

    check.foreach { checkData =>
      val matched = logData == checkData
      if(!matched) Assert.fail("Output does not match: See "+basename+"(.check,-run.log)")
    }
  }

  def read(f: File): String = {
    val buf = new StringBuilder
    val a = new Array[Char](4096)
    val in = new FileInputStream(f)
    try {
      val r = new InputStreamReader(in, "UTF-8")
      var num: Int = 0
      while({ num = r.read(a); num > 0}) buf.append(new String(a, 0, num))
      buf.toString()
    } finally in.close
  }
}

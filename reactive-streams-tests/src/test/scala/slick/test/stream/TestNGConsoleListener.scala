package slick.test.stream

import org.testng.{ITestResult, TestListenerAdapter}

import slick.util.GlobalConfig

class TestNGConsoleListener extends TestListenerAdapter {
  val (normal, yellow, blue, cyan, red) =
    if(GlobalConfig.ansiDump) ("\u001B[0m", "\u001B[33m", "\u001B[34m", "\u001B[36m", "\u001B[31m")
    else ("", "", "", "", "")

  override def onTestFailure(tr: ITestResult): Unit = {
    printError(tr, tr.getThrowable, "failed", red)
  }

  override def onTestSuccess(tr: ITestResult): Unit = {
    printError(tr, null, "succeeded", cyan)
  }

  override def onTestSkipped(tr: ITestResult): Unit = {
    printError(tr, null, "skipped", blue)
  }

  def printError(tr: ITestResult, t: Throwable, msg: String, highlight: String): Unit = {
    val cln = tr.getTestClass.getName
    val sep = cln.lastIndexOf('.')
    val cln2 = if(sep == -1) (yellow + cln) else cln.substring(0, sep+1) + yellow + cln.substring(sep+1)
    val mn = tr.getMethod.getMethodName
    val param = tr.getParameters.map(_.toString).mkString(",")
    val param2 = if(param == "") "" else s"[$yellow$param$normal]"
    print(s"Test $cln2$normal.$highlight$mn$normal$param2 $msg")
    if(t eq null) println()
    else {
      print(": ")
      t.printStackTrace(System.out)
    }
  }
}

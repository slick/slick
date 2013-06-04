package scala.slick.typeproviders

import java.io.PrintWriter
import java.io.File

object CodeGeneratorMain {
  def main(args: Array[String]) {
    args.toList match {
      case List(configFileName, outputFolder, fullName) => {
        val cg = new CodeGenerator(configFileName)
        val (className, packageName) = {
          val lastPoint = fullName.lastIndexOf(".")
          if (lastPoint == -1)
            (fullName, None)
          else {
            (fullName.substring(lastPoint + 1), Some(fullName.substring(0, lastPoint)))
          }
        }
        val content = cg.generateCode(className, packageName)
        val packageFolder = packageName.map(n => n.replace('.', '/') + "/").getOrElse("")
        val outputFileName = s"$outputFolder/$packageFolder$className.scala"
        val outputFile = new File(outputFileName)
        outputFile.getParentFile().mkdirs()
        val pw = new PrintWriter(outputFile)
        pw.println(content)
        pw.close()
        println(s"Your generated code is dumped to $outputFileName")
      }
      case _ => {
        println("""
You have to use Code Generator with these parameters:
   <configFileName> 
   <outputFolder>
   <className>
            """.trim
        )
      }
    }
  }
}

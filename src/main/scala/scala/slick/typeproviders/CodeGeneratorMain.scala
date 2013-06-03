package scala.slick.typeproviders

import java.io.PrintWriter

object CodeGeneratorMain {
  def main(args: Array[String]) {
    args.toList match {
      case List(configFileName, outputFolder, className) => {
        val cg = new CodeGenerator(configFileName)
        val content = cg.generateCode(className)
        val outputFileName = s"$outputFolder/$className.scala"
        val pw = new PrintWriter(outputFileName)
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

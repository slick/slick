package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.typeproviders.CodeGenerator

class CodeGeneratorTest {
  @Test def simpleTest() {
    val codeGen = new CodeGenerator("type-providers-h2mem")
    import codeGen.macroHelper
    println(codeGen.generateCode())
  }
}
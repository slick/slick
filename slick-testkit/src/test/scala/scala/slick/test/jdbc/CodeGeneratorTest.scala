package scala.slick.test.jdbc

import org.junit.Test
import org.junit.Assert._
import scala.slick.typeproviders.CodeGenerator

class CodeGeneratorTest {
  val testDbs = "test-dbs/type-provider/conf/"
  @Test def simpleTest() {
    val names = List("type-providers-h2mem", "type-providers-hsql", "type-providers-sqlite", "type-providers-h2mem-fk-1",
      "type-providers-h2mem-fk-2", "type-providers-h2mem-ainc", "type-providers-h2mem-custom-naming", "type-providers-h2mem-custom-typing")
    //    val names = List("type-providers-h2mem-custom-typing")
    var count = 1
    for (name <- names) {
      val codeGen = new CodeGenerator(testDbs + name)
      println(s"//code for $name")
      println(codeGen.generateCode("Db" + count))
      count += 1
    }
  }
}
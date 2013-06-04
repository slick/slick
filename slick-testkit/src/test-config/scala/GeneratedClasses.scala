package scala.slick.typeproviders.test

import scala.slick.typeproviders.CodeGeneratorMain

object GeneratedClasses {
  def main(args: Array[String]) {
    val outputDir = args(0)
    val tests = Map("type-providers-h2mem" -> "CG1",
      "type-providers-hsql" -> "CG2",
      "type-providers-sqlite" -> "CG3",
      "type-providers-h2mem-fk-1" -> "CG4",
      "type-providers-h2mem-fk-2" -> "CG5",
      "type-providers-h2mem-ainc" -> "CG6",
      "type-providers-h2mem-custom-naming" -> "CG7",
      "type-providers-h2mem-custom-typing" -> "CG8"
    )
    for (t <- tests) {
      CodeGeneratorMain.main(Array("test-dbs/type-provider/conf/" + t._1, outputDir, "scala.slick.test.cg." + t._2))
    }
  }
}
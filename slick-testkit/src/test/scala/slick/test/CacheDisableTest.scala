package slick.test

import slick.compiler.CompilationCache

/** Test to verify cache can be disabled during tests */
object CacheDisableTest {
  def main(args: Array[String]): Unit = {
    println(s"Cache enabled: ${CompilationCache.isEnabled}")
    println(s"System property slick.testRun: ${sys.props.get("slick.testRun")}")
    
    val expectedDisabled = sys.props.get("slick.testRun").exists(_.toLowerCase == "true")
    val actualEnabled = CompilationCache.isEnabled
    
    if (expectedDisabled && actualEnabled) {
      println("ERROR: Cache should be disabled during tests but it's enabled!")
      sys.exit(1)
    } else if (!expectedDisabled && !actualEnabled) {
      println("ERROR: Cache should be enabled but it's disabled!")
      sys.exit(1)
    } else {
      println("SUCCESS: Cache state is correct!")
    }
  }
}
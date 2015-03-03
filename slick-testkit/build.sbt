name := "Slick-TestKit"
description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)"
scalacOptions in(Compile, doc) <++= version.map(v => Seq(
  "-doc-source-url", "https://github.com/slick/slick/blob/" + v + "/slick-testkit/src/main€{FILE_PATH}.scala"
))
testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a", "-Djava.awt.headless=true")
//scalacOptions in Compile += "-Yreify-copypaste",
libraryDependencies ++= Seq(
  Library.junit,
  Library.junitInterface,
  Library.logback % Test,
  Library.reactiveStreamsTCK % Test
)
libraryDependencies ++= Library.DB.all.map(_ % Test)
libraryDependencies ++= Library.DB.all.map(_ % "codegen")
// Run the Queryable tests (which need macros) on a forked JVM
// to avoid classloader problems with reification
testGrouping <<= definedTests in Test map partitionTests
// Workaround for sbt bug: Without a testGrouping for all test configs,
// the wrong tests are run
//      testGrouping in DocTest <<= definedTests in DocTest map partitionTests, //FIXME
fork in run := true
javaOptions in run += "-Dslick.ansiDump=true"
//javaOptions in run += "-verbose:gc",
compile in Test ~= { a =>
  // Delete classes in "compile" packages after compiling. (Currently only scala.slick.test.compile.NestedShapeTest)
  // These are used for compile-time tests and should be recompiled every time.
  val products = a.relations.allProducts.toSeq ** new SimpleFileFilter(_.getParentFile.getName == "compile")
  IO.delete(products.get)
  a
}
buildCapabilitiesTable := {
  val logger = ConsoleLogger()
  Run.run("com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
    (fullClasspath in Compile).value.map(_.data),
    Seq("src/sphinx/capabilities.csv"),
    logger)(runner.value)
}
libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "provided")

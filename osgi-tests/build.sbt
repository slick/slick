name := "Slick-OsgiTests"
libraryDependencies ++= Seq(
  Library.logback % Test,
  Library.junit % Test,
  Library.junitInterface % Test,
  Library.reactiveStreams % Test,
  Library.DB.h2 % Test
)
libraryDependencies ++= Dependencies.paxExam.map(_ % Test)
unmanagedResourceDirectories in Test += (baseDirectory in root).value / "common-test-resources"
fork in Test := true
testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")
javaOptions in Test ++= Seq(
  "-Dslick.osgi.bundlepath=" + osgiBundleFiles.value.map(_.getCanonicalPath).mkString(":"),
  "-Dorg.ops4j.pax.logging.DefaultServiceLog.level=WARN"
)
testGrouping <<= definedTests in Test map partitionTests
osgiBundleFiles := Seq((OsgiKeys.bundle in slick).value)
osgiBundleFiles ++= (dependencyClasspath in Compile in slick).value.map(_.data).filterNot(_.isDirectory)
osgiBundleFiles ++= (dependencyClasspath in Test).value.map(_.data).filter(f => f.name.contains("logback-") || f.name.contains("h2") || f.name.contains("reactive-streams"))
publishArtifact := false

scalaVersion := "2.12.1"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "com.typesafe.slick" %% "slick-testkit" % "3.2.0" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "ch.qos.logback" % "logback-classic" % "1.1.3" % "test",
  "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test"
)

scalacOptions += "-deprecation"

parallelExecution in Test := false

logBuffered := false

fork in run := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

scalaVersion := "2.12.8"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.3.0",
  "com.typesafe.slick" %% "slick-testkit" % "3.3.0" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "test",
  "org.postgresql" % "postgresql" % "42.1.4" % "test"
)

scalacOptions += "-deprecation"

parallelExecution in Test := false

logBuffered := false

fork in run := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

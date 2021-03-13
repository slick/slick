scalaVersion := "2.12.13"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.2.3",
  "com.typesafe.slick" %% "slick-testkit" % "3.2.3" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "test",
  "org.postgresql" % "postgresql" % "42.2.5" % "test"
)

scalacOptions += "-deprecation"

Test / parallelExecution := false

logBuffered := false

run / fork := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

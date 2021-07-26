libraryDependencies ++= List(
  "com.novocode" % "junit-interface" % "0.11" % Test,
  "ch.qos.logback" % "logback-classic" % "1.2.5" % Test,
  "org.postgresql" % "postgresql" % "42.2.23" % Test,
)

scalacOptions += "-deprecation"

Test / parallelExecution := false

logBuffered := false

run / fork := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

libraryDependencies ++= List(
  "org.slf4j" % "slf4j-nop" % "1.7.26",
  "com.h2database" % "h2" % "1.4.200",
  "org.scalatest" %% "scalatest" % "3.2.6" % Test
)

scalacOptions += "-deprecation"
run / fork := true

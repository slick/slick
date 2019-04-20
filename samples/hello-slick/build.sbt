scalaVersion := "2.12.8"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.3.0",
  "org.slf4j" % "slf4j-nop" % "1.7.25",
  "com.h2database" % "h2" % "1.4.191",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

scalacOptions += "-deprecation"

fork in run := true

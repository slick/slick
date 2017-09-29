scalaVersion := "2.12.1"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "org.slf4j" % "slf4j-nop" % "1.7.10",
  "com.h2database" % "h2" % "1.4.187",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions += "-deprecation"

fork in run := true

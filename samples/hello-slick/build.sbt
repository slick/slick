scalaVersion := "2.12.10"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.3.0",
  "org.slf4j" % "slf4j-nop" % "1.7.26",
  "com.h2database" % "h2" % "1.4.199",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

scalacOptions += "-deprecation"

fork in run := true

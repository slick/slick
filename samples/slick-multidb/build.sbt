scalaVersion := "2.12.13"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.2.3",
  "org.slf4j" % "slf4j-nop" % "1.7.26",
  "com.h2database" % "h2" % "1.4.200",
  "org.xerial" % "sqlite-jdbc" % "3.34.0"
)

scalacOptions += "-deprecation"

run / fork := true

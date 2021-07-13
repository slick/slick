libraryDependencies ++= List(
  "org.slf4j" % "slf4j-nop" % "1.7.26",
  "com.h2database" % "h2" % "1.4.200",
  "com.typesafe.slick" %% "slick" % "3.2.3"
)

scalacOptions += "-deprecation"

run / fork := true
scalaVersion := "2.12.14"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

Compile / unmanagedClasspath ++= (Compile / unmanagedResources).value

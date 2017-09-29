scalaVersion := "2.12.1"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "org.slf4j" % "slf4j-nop" % "1.7.10",
  "com.h2database" % "h2" % "1.4.187"
)

scalacOptions += "-deprecation"

fork in run := true

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

unmanagedClasspath in Compile ++= (unmanagedResources in Compile).value

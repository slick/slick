scalaVersion := "2.12.4"

libraryDependencies ++= List(
  "com.typesafe.slick" %% "slick" % "3.2.3",
  "org.slf4j" % "slf4j-nop" % "1.7.25",
  "com.h2database" % "h2" % "1.4.191"
)

scalacOptions += "-deprecation"

fork in run := true

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

unmanagedClasspath in Compile ++= (unmanagedResources in Compile).value

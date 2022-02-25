libraryDependencies ++= List(
  "org.slf4j" % "slf4j-nop" % "1.7.26",
  "com.h2database" % "h2" % "2.1.210",
  "org.xerial" % "sqlite-jdbc" % "3.36.0.3"
)

scalacOptions += "-deprecation"

run / fork := true

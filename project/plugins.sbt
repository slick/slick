scalacOptions ++= Seq("-deprecation", "-feature")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")
addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")
addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.6")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "2.1.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.11")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox" % "0.10.3")

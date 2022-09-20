scalacOptions ++= Seq("-deprecation", "-feature")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")
addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")
addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.4")
addSbtPlugin("ch.epfl.scala" % "sbt-version-policy" % "2.0.1")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")
addSbtPlugin("com.lightbend.paradox" % "sbt-paradox" % "0.10.2")

scalacOptions += "-deprecation"

scalacOptions += "-feature"

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.1")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.1")

addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.0.2")

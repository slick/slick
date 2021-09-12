scalacOptions ++= Seq("-deprecation", "-feature")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.0.0")
addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")
addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")
addSbtPlugin("com.novocode" % "sbt-ornate" % "0.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.8.2")
addSbtPlugin("io.isomarcte" % "sbt-version-scheme-enforcer-plugin" % "2.1.0.3")

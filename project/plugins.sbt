scalacOptions ++= Seq("-deprecation", "-feature")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.9.2")

addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")

addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")

addSbtPlugin("com.novocode" % "sbt-ornate" % "0.6")

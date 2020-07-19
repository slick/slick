scalacOptions += "-deprecation"

scalacOptions += "-feature"

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.4")

addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")

addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")

resolvers += Resolver.url("fix-sbt-plugin-releases", url("https://dl.bintray.com/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.novocode" % "sbt-ornate" % "0.6")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.1")

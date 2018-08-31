scalacOptions += "-deprecation"

scalacOptions += "-feature"

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.18")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.1")

addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")

resolvers += Resolver.url("fix-sbt-plugin-releases", url("https://dl.bintray.com/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.novocode" % "sbt-ornate" % "0.5")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

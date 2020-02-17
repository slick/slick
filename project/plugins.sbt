scalacOptions ++= Seq("-deprecation", "-feature")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.3.0")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.5")

addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")

addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")

resolvers += Resolver.url("fix-sbt-plugin-releases", url("https://dl.bintray.com/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.novocode" % "sbt-ornate" % "0.6")

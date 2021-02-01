scalacOptions ++= Seq("-deprecation", "-feature")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.6")

addSbtPlugin("com.typesafe" % "sbt-sdlc" % "0.2")

addSbtPlugin("de.johoop" % "sbt-testng-plugin" % "3.1.1")

resolvers += Resolver.url("fix-sbt-plugin-releases", url("https://dl.bintray.com/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.novocode" % "sbt-ornate" % "0.6")

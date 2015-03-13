name := "Slick-Direct"
description := "Direct Embedding for Slick (Scala Language-Integrated Connection Kit)"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
scalacOptions in (Compile, doc) ++= Seq(
  "-doc-source-url", "https://github.com/slick/slick/blob/"+version.value+"/slick-direct/src/main€{FILE_PATH}.scala"
)
test := ()
testOnly :=  () // suppress test status output
//sdlcBase := "direct-api/"
//sdlcCheckDir := (target in (slickProject, com.typesafe.sbt.SbtSite.SiteKeys.makeSite)).value
//sdlc <<= sdlc dependsOn (doc in Compile, com.typesafe.sbt.SbtSite.SiteKeys.makeSite in slickProject)

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{previousArtifact, binaryIssueFilters}
import com.typesafe.tools.mima.core.{ProblemFilters, MissingClassProblem}
name := "Slick"
description := "Scala Language-Integrated Connection Kit"
scalacOptions in(Compile, doc) <++= version.map(v => Seq(
  "-doc-source-url", "https://github.com/slick/slick/blob/" + v + "/src/main€{FILE_PATH}.scala",
  "-doc-root-content", "scaladoc-root.txt"
))
//      makeSite <<= makeSite dependsOn (buildCapabilitiesTable in slickTestkitProject), //FIXME
//      sdlcBase := "api/",
//      sdlcCheckDir := (target in com.typesafe.sbt.SbtSite.SiteKeys.makeSite).value,
//      sdlc <<= sdlc dependsOn (doc in Compile, com.typesafe.sbt.SbtSite.SiteKeys.makeSite),
test :=()
testOnly :=() // suppress test status output
previousArtifact := Some("com.typesafe.slick" %% "slick" % binaryCompatSlickVersion)
binaryIssueFilters ++= Seq(
  ProblemFilters.exclude[MissingClassProblem]("scala.slick.util.MacroSupportInterpolationImpl$"),
  ProblemFilters.exclude[MissingClassProblem]("scala.slick.util.MacroSupportInterpolationImpl")
)
ivyConfigurations += config("macro").hide.extend(Compile)
unmanagedClasspath in Compile <++= fullClasspath in config("macro")
mappings in(Compile, packageSrc) <++= mappings in(config("macro"), packageSrc)
mappings in(Compile, packageBin) <++= mappings in(config("macro"), packageBin)
OsgiKeys.exportPackage := Seq("scala.slick", "scala.slick.*")
OsgiKeys.importPackage := Seq(
  osgiImport("scala*", scalaVersion.value),
  "*"
)
OsgiKeys.privatePackage := Nil
libraryDependencies ++= Seq(
  Library.slf4j,
  Library.typesafeConfig,
  Library.reactiveStreams,
  Library.hikariCP % Optional
)
libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "macro")

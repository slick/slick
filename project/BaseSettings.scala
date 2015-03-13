import sbt.Keys._
import sbt._

object BaseSettings extends AutoPlugin {
  override val trigger = allRequirements

  override val projectSettings = Seq(
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    version := SlickBuild.slickVersion,
    scalaVersion := SlickBuild.scalaVersions.head,
    crossScalaVersions := SlickBuild.scalaVersions,
    homepage := Some(url("http://slick.typesafe.com")),
    startYear := Some(2008),
    licenses +=("Two-clause BSD-style license", url("http://github.com/slick/slick/blob/master/LICENSE.txt")),
    ivyLoggingLevel := UpdateLogging.DownloadOnly,
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= List("-deprecation", "-feature"),
    scalacOptions in(Compile, doc) ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (sourceDirectory in Compile).value.getPath, // needed for scaladoc to strip the location of the linked source path
      "-doc-source-url", "https://github.com/slick/slick/blob/" + version.value + "/src/main€{FILE_PATH}.scala",
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    ),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a", "-Djava.awt.headless=true"),
    parallelExecution in Test := false,
    logBuffered := false
  )

}

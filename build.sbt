import Settings._
import Docs.docDir
import BuildUtils._

ThisBuild / version := "3.4.0-SNAPSHOT"

// Slick base version for binary compatibility checks.
// The next release to be cut from master will be 3.4.0 during develop of 3.4.0 we check compatibility with 3.3.0.
// The goal is not to stop any breaking change, but to make us aware. For each breaking change we should add MiMa exclusions.
// This will also help us decide when a PR can be backported in 3.3.x branch.
ThisBuild / binaryCompatSlickVersion := None

ThisBuild / docDir := (aRootProject / baseDirectory).value / "doc"

lazy val overridesForSampleProjects: State => State = { s: State =>
  "project sample-hello-slick" :: "update-sample" ::
  "project sample-slick-multidb" :: "update-sample" ::
  "project sample-slick-plainsql" :: "update-sample" ::
  "project sample-slick-testkit-example" :: "update-sample" ::
  "project root" :: s
}

Global / onLoad := { state =>
  if (state.get(sampleOverridden) getOrElse false)
    (Global / onLoad).value(state)
  else {
    val old = (Global / onLoad).value
    (overridesForSampleProjects compose old)(state)
  }
}

lazy val slickProject: Project = Project(id = "slick", base =  file("slick")).settings(slickProjectSettings).enablePlugins(SbtOsgi, SDLCPlugin)

lazy val slickTestkitProject = Project(id = "testkit", base = file("slick-testkit")).settings(slickTestkitProjectSettings).configs(DocTest).enablePlugins(SDLCPlugin).
    dependsOn(slickProject,
              slickCodegenProject % "compile->compile",
              slickHikariCPProject)

lazy val slickCodegenProject = Project(id = "codegen", base = file("slick-codegen")).settings(slickCodegenProjectSettings).enablePlugins(SDLCPlugin).
    dependsOn(slickProject)

lazy val slickHikariCPProject = Project(id = "hikaricp", base = file("slick-hikaricp")).settings(slickHikariCPProjectSettings).enablePlugins(SbtOsgi, SDLCPlugin).
    dependsOn(slickProject)

lazy val reactiveStreamsTestProject = Project(id = "reactive-streams-tests", base = file("reactive-streams-tests")).settings(reactiveStreamsTestProjectSettings).
    dependsOn(slickTestkitProject)

lazy val osgiTestProject = Project(id = "osgitests", base = file("osgi-tests")).settings(osgiTestProjectSettings).
    dependsOn(slickProject % "test")

lazy val aRootProject: Project = Project(id = "root", base = file(".")).settings(aRootProjectSettings).
    settings(
      commands += testAll,
      testSamples := {
        val __ = Def.sequential(
          sampleHelloSlickProject / Test / test,
          (sampleHelloSlickProject / Compile / runMain).toTask(" HelloSlick"),
          (sampleHelloSlickProject / Compile / runMain).toTask(" CaseClassMapping"),
          (sampleHelloSlickProject / Compile / runMain).toTask(" QueryActions"),
          (sampleSlickPlainsqlProject / Compile / runMain).toTask(" PlainSQL"),
          (sampleSlickPlainsqlProject / Compile / runMain).toTask(" TypedSQL"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" SimpleExample"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" MultiDBExample"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" MultiDBCakeExample"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" CallNativeDBFunction"),
          sampleSlickTestkitExampleProject / Test / compile // running would require external setup
        ).value
        ()
      }
    ).enablePlugins(OrnatePlugin, SbtOsgi).
    aggregate(slickProject,
              slickCodegenProject,
              slickHikariCPProject,
              slickTestkitProject)

// sample projects under ./samples
lazy val sampleHelloSlickProject =
  sampleProject("hello-slick").dependsOn(slickProject)

lazy val sampleSlickMultidbProject =
  sampleProject("slick-multidb").dependsOn(slickProject)

lazy val sampleSlickPlainsqlProject =
  sampleProject("slick-plainsql").dependsOn(slickProject)

lazy val sampleSlickTestkitExampleProject =
  sampleProject("slick-testkit-example").dependsOn(slickProject, slickTestkitProject)

/* A command that runs all tests sequentially */
def testAll = Command.command("testAll") { state =>
  val extracted = Project.extract(state)

  // 0.13.8; use Def.sequential?
  val tasks = List(
    slickTestkitProject / Test / test,
    slickTestkitProject / DocTest / test,
    osgiTestProject / Test / test,
    reactiveStreamsTestProject / Test / test,
    slickProject / Compile / packageDoc,
    slickCodegenProject / Compile / packageDoc,
    slickHikariCPProject / Compile / packageDoc,
    slickTestkitProject / Compile / packageDoc,
    slickProject / Compile / mimaReportBinaryIssues, // enable for minor versions
    aRootProject / testSamples
  )

  /* val withSdlc =
   if(extracted.get(scalaVersion).startsWith("2.11.")) tasks :+ (sdlc in aRootProject)
   else tasks */

  runTasksSequentially(tasks)(state)
}

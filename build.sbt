import Settings.{testSample1, testSample2, testSample3, testSample4, _}
import Docs.docDir
import BuildUtils._

ThisBuild / version := "3.4.0-SNAPSHOT"

// Slick base version for binary compatibility checks.
// The next release to be cut from master will be 3.4.0 during develop of 3.4.0 we check compatibility with 3.3.0.
// The goal is not to stop any breaking change, but to make us aware. For each breaking change we should add MiMa exclusions.
// This will also help us decide when a PR can be backported in 3.3.x branch.
ThisBuild / binaryCompatSlickVersion := {
  if (scalaBinaryVersion.value.startsWith("2.13")) None else Some("3.3.0")
}

ThisBuild / docDir := (aRootProject / baseDirectory).value / "doc"

lazy val overridesForSampleProjects: State => State = { s: State =>
  "project sample-hello-slick" :: "update-sample" ::
  "project sample-slick-multidb" :: "update-sample" ::
  "project sample-slick-plainsql" :: "update-sample" ::
  "project sample-slick-testkit-example" :: "update-sample" ::
  "project root" :: s
}

lazy val updateSampleHelloSlick: State => State = { s: State =>
  "project sample-hello-slick" :: "update-sample" :: s
}

lazy val updateSampleSlickMultidb: State => State = { s: State =>
  "project sample-slick-multidb" :: "update-sample" :: s
}

lazy val updateSampleSlickPlainsql: State => State = { s: State =>
  "project sample-slick-plainsql" :: "update-sample" :: s
}

lazy val updateSampleSlickTestkitExample: State => State = { s: State =>
  "project sample-slick-testkit-example" :: "update-sample" :: s
}


Global / onLoad := { state =>
  if (state.get(sampleOverridden) getOrElse false)
    (Global / onLoad).value(state)
  else {
    val old = (Global / onLoad).value
    (overridesForSampleProjects compose old)(state)
  }
}

lazy val slickProject: Project = Project(id = "slick", base =  file("slick")).settings(slickProjectSettings).enablePlugins(SDLCPlugin)

lazy val slickTestkitProject = Project(id = "testkit", base = file("slick-testkit")).settings(slickTestkitProjectSettings).configs(DocTest).enablePlugins(SDLCPlugin).
    dependsOn(slickProject,
              slickCodegenProject % "compile->compile",
              slickHikariCPProject)

lazy val slickCodegenProject = Project(id = "codegen", base = file("slick-codegen")).settings(slickCodegenProjectSettings).enablePlugins(SDLCPlugin).
    dependsOn(slickProject)

lazy val slickHikariCPProject = Project(id = "hikaricp", base = file("slick-hikaricp")).settings(slickHikariCPProjectSettings).enablePlugins(SDLCPlugin).
    dependsOn(slickProject)

lazy val reactiveStreamsTestProject = Project(id = "reactive-streams-tests", base = file("reactive-streams-tests")).settings(reactiveStreamsTestProjectSettings).
    dependsOn(slickTestkitProject)

lazy val aRootProject: Project = Project(id = "root", base = file(".")).settings(aRootProjectSettings).
    settings(
      commands ++= Seq(testAll,
        Command.command("updateSampleHelloSlick")(updateSampleHelloSlick),
        Command.command("updateSampleSlickPlainsql")(updateSampleSlickPlainsql),
        Command.command("updateSampleSlickMultidb")(updateSampleSlickMultidb),
        Command.command("updateSampleSlickTestkitExample")(updateSampleSlickTestkitExample),

      ),
      testSample1 := {
        val __ = Def.sequential(
          sampleHelloSlickProject / Test / test,
          (sampleHelloSlickProject / Compile / runMain).toTask(" HelloSlick"),
          (sampleHelloSlickProject / Compile / runMain).toTask(" CaseClassMapping"),
          (sampleHelloSlickProject / Compile / runMain).toTask(" QueryActions")
        ).value
        ()
      },
      testSample2 := {
        val __ = Def.sequential(
          (sampleSlickPlainsqlProject / Compile / runMain).toTask(" PlainSQL"),
          (sampleSlickPlainsqlProject / Compile / runMain).toTask(" TypedSQL")
        ).value
        ()
      },
      testSample3 := {
        val __ = Def.sequential(
          (sampleSlickMultidbProject / Compile / runMain).toTask(" SimpleExample"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" MultiDBExample"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" MultiDBCakeExample"),
          (sampleSlickMultidbProject / Compile / runMain).toTask(" CallNativeDBFunction")
        ).value
        ()
      },
      testSample4 := {
        val __ = Def.sequential(
          sampleSlickTestkitExampleProject / Test / compile // running would require external setup
        ).value
        ()
      },
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
      },
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 12)) => libraryDependencies.value
          case _ => libraryDependencies.value.filter(!_.configurations.contains(Ornate.name))
        }
      }
    ).enablePlugins(OrnatePlugin).
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
    reactiveStreamsTestProject / Test / test,
    slickProject / Compile / packageDoc,
    slickCodegenProject / Compile / packageDoc,
    slickHikariCPProject / Compile / packageDoc,
    slickTestkitProject / Compile / packageDoc,
    slickProject / Compile / mimaReportBinaryIssues // enable for minor versions
//    aRootProject / testSamples
  )

  /* val withSdlc =
   if(extracted.get(scalaVersion).startsWith("2.11.")) tasks :+ (sdlc in aRootProject)
   else tasks */

  runTasksSequentially(tasks)(state)
}

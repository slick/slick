import Settings._
import Docs.docDir
import BuildUtils._

version in ThisBuild := "3.4.0-SNAPSHOT"

// Slick base version for binary compatibility checks.
// The next release to be cut from master will be 3.4.0 during develop of 3.4.0 we check compatibility with 3.3.0.
// The goal is not to stop any breaking change, but to make us aware. For each breaking change we should add MiMa exclusions.
// This will also help us decide when a PR can be backported in 3.3.x branch.  
binaryCompatSlickVersion in ThisBuild := Some("3.3.0") 

docDir in ThisBuild := (baseDirectory in aRootProject).value / "doc"

lazy val slickProject: Project =
  Project(id = "slick", base = file("slick"),
          settings = slickProjectSettings)

lazy val slickTestkitProject =
  Project(id = "testkit", base = file("slick-testkit"),
          settings = slickTestkitProjectSettings).
    configs(DocTest).
    dependsOn(slickProject,
              slickCodegenProject % "compile->compile",
              slickHikariCPProject)

lazy val slickCodegenProject =
  Project(id = "codegen", base = file("slick-codegen"),
          settings = slickCodegenProjectSettings).
    dependsOn(slickProject)

lazy val slickHikariCPProject =
  Project(id = "hikaricp", base = file("slick-hikaricp"),
          settings = slickHikariCPProjectSettings).
    dependsOn(slickProject)

lazy val reactiveStreamsTestProject =
  Project(id = "reactive-streams-tests", base = file("reactive-streams-tests"),
          settings = reactiveStreamsTestProjectSettings).
    dependsOn(slickTestkitProject)

lazy val osgiTestProject =
  Project(id = "osgitests", base = file("osgi-tests"),
          settings = osgiTestProjectSettings).
    dependsOn(slickProject % "test")

lazy val aRootProject: Project =
  Project(id = "root", base = file("."),
          settings = aRootProjectSettings).
    settings(
      commands += testAll,
      testSamples := {
        val __ = Def.sequential(
          test in (sampleHelloSlickProject, Test),
          (runMain in Compile in sampleHelloSlickProject).toTask(" HelloSlick"),
          (runMain in Compile in sampleHelloSlickProject).toTask(" CaseClassMapping"),
          (runMain in Compile in sampleHelloSlickProject).toTask(" QueryActions"),
          (runMain in Compile in sampleSlickPlainsqlProject).toTask(" PlainSQL"),
          (runMain in Compile in sampleSlickPlainsqlProject).toTask(" TypedSQL"),
          (runMain in Compile in sampleSlickMultidbProject).toTask(" SimpleExample"),
          (runMain in Compile in sampleSlickMultidbProject).toTask(" MultiDBExample"),
          (runMain in Compile in sampleSlickMultidbProject).toTask(" MultiDBCakeExample"),
          (runMain in Compile in sampleSlickMultidbProject).toTask(" CallNativeDBFunction"),
          compile in (sampleSlickTestkitExampleProject, Test) // running would require external setup
        ).value
        ()
      }
    ).
    enablePlugins(OrnatePlugin).
    aggregate(slickProject,
              slickCodegenProject,
              slickHikariCPProject,
              slickTestkitProject
    )

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
  val tasks = List(
    test in (slickTestkitProject, Test),
    test in (slickTestkitProject, DocTest),
    test in (osgiTestProject, Test),
    test in (reactiveStreamsTestProject, Test),
    packageDoc in Compile in slickProject,
    packageDoc in Compile in slickCodegenProject,
    packageDoc in Compile in slickHikariCPProject,
    packageDoc in Compile in slickTestkitProject,
    mimaReportBinaryIssues in Compile in slickProject, // enable for minor versions
    testSamples in aRootProject
  )

  /* val withSdlc =
   if(extracted.get(scalaVersion).startsWith("2.11.")) tasks :+ (sdlc in aRootProject)
   else tasks */

  runTasksSequentially(tasks)(state)
}


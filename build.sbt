import Docs.docDir
import com.jsuereth.sbtpgp.PgpKeys


val testAll = taskKey[Unit]("Run all tests")
val testAllSamples = taskKey[Unit]("Run tests in the all sample apps")
val testSampleHelloSlick = taskKey[Unit]("Run tests in the hello-slick sample app")
val testSamplePlainSql = taskKey[Unit]("Run tests in the plain-sql sample app")
val testSampleMultiDb = taskKey[Unit]("Run tests in the multidb sample app")
val testSampleTestkit = taskKey[Unit]("Run tests in the testkit-example sample app")

val cleanCompileTimeTests =
  taskKey[Unit]("Delete files used for compile-time tests which should be recompiled every time.")

val repoKind = settingKey[String]("""Maven repository kind ("snapshots" or "releases")""")

/* Test Configuration for running tests on doc sources */
val DocTest = config("doctest").extend(Test)
val MacroConfig = config("macro")


def scaladocSourceUrl(dir: String) =
  Compile / doc / scalacOptions ++= Seq(
    "-doc-source-url",
    s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/$dir/src/main€{FILE_PATH}.scala"
  )

def slickGeneralSettings =
  Seq(
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    repoKind := (if (version.value.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"),
    publishTo :=
      (repoKind.value match {
        case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
        case "releases"  => Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
      }),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= {
      _.withConfigurations(Vector(Compile, Runtime, Optional))
    },
    homepage := Some(url("https://slick.typesafe.com")),
    startYear := Some(2008),
    licenses += ("Two-clause BSD-style license", url("https://github.com/slick/slick/blob/master/LICENSE.txt")),
    developers :=
      List(
        Developer("szeiger", "Stefan Zeiger", "", url("http://szeiger.de")),
        Developer("hvesalai", "Heikki Vesalainen", "", url("https://github.com/hvesalai/"))
      ),
    scmInfo := Some(ScmInfo(url("https://github.com/slick/slick"), "scm:git:git@github.com:slick/slick.git")),
    scalacOptions ++= List("-deprecation", "-feature", "-unchecked"),
    scalacOptions ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 => Seq("-Xfuture")
        case _                       => Nil
      }),
    Compile / doc / scalacOptions ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (Compile / sourceDirectory).value.getPath, // make scaladoc strip location of linked source path
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    ),
    scaladocSourceUrl("slick"),
    logBuffered := false
  )

// set the scala-compiler dependency unless a local scala is in use
def compilerDependencySetting(config: String) =
  if (sys.props("scala.home.local") != null) Nil else Seq(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % config
  )

def extTarget(extName: String): Seq[Setting[File]] =
  sys.props("slick.build.target") match {
    case null => Seq.empty
    case path => Seq(target := file(path + "/" + extName))
  }

def commonTestResourcesSetting =
  Test / unmanagedResourceDirectories +=
    (LocalProject("root") / baseDirectory).value / "common-test-resources"

def sampleSettings = Seq(
  Compile / unmanagedClasspath :=
    Attributed.blank(baseDirectory.value.getParentFile / "resources") +: (Compile / unmanagedClasspath).value,
  Compile / unmanagedClasspath ++= (slick / MacroConfig / products).value
)

ThisBuild / version := "3.4.0-SNAPSHOT"

ThisBuild / crossScalaVersions := Dependencies.scalaVersions
ThisBuild / scalaVersion := Dependencies.scalaVersions.last

ThisBuild / versionScheme := Some("pvp")

ThisBuild / docDir := (root / baseDirectory).value / "doc"


lazy val slick =
  project
    .enablePlugins(SDLCPlugin, MimaPlugin)
    .settings(
      slickGeneralSettings,
      compilerDependencySetting("macro"),
      inConfig(MacroConfig)(Defaults.configSettings),
      FMPP.preprocessorSettings,
      extTarget("slick"),
      Docs.scaladocSettings,

      name := "Slick",
      description := "Scala Language-Integrated Connection Kit",
      libraryDependencies ++= Dependencies.mainDependencies,
      scaladocSourceUrl("slick"),
      Compile / doc / scalacOptions ++= Seq(
        "-doc-root-content", "scaladoc-root.txt"
      ),

      // suppress test status output
      test := {},
      testOnly := {},

      ivyConfigurations += MacroConfig.hide.extend(Compile),
      Compile / unmanagedClasspath ++= (MacroConfig / products).value,
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      (Compile / packageSrc / mappings) ++= (MacroConfig / packageSrc / mappings).value,
      (Compile / packageBin / mappings) ++= (MacroConfig / packageBin / mappings).value
    )

lazy val testkit =
  project
    .in(file("slick-testkit"))
    .configs(DocTest)
    .enablePlugins(SDLCPlugin)
    .dependsOn(slick, codegen % "compile->compile", hikaricp)
    .settings(
      slickGeneralSettings,
      compilerDependencySetting("provided"),
      inConfig(DocTest)(Defaults.testSettings),
      Docs.scaladocSettings,
      TypeProviders.codegenSettings,
      extTarget("testkit"),
      name := "Slick-TestKit",
      description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
      scaladocSourceUrl("slick-testkit"),
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a", "-Djava.awt.headless=true"),
      //scalacOptions in Compile += "-Yreify-copypaste",
      libraryDependencies ++=
        Dependencies.junit ++:
          (Dependencies.reactiveStreamsTCK % Test) +:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % Test) ++:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % "codegen"),
      Test / parallelExecution := false,
      run / fork := true,
      //connectInput in run := true,
      run / javaOptions += "-Dslick.ansiDump=true",
      //javaOptions in run += "-verbose:gc",
      // Delete classes in "compile" packages after compiling. (Currently only slick.test.compile.NestedShapeTest)
      // These are used for compile-time tests and should be recompiled every time.
      Test / cleanCompileTimeTests := {
        val products = fileTreeView.value.list(
          (Test / classDirectory).value.toGlob / ** / "compile" / *
        ).map(x => x._1.toFile)
        streams.value.log.info(s"Deleting $products")
        IO.delete(products)
      },
      (Test / cleanCompileTimeTests) := ((Test / cleanCompileTimeTests) triggeredBy (Test / compile)).value,
      Docs.buildCapabilitiesTable := {
        val logger = ConsoleLogger()
        Run.run("com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
          (Compile / fullClasspath).value.map(_.data),
          Seq(Docs.docDir.value / "capabilities.md") map (_.toString),
          logger)(runner.value)
      },
      DocTest / unmanagedSourceDirectories += Docs.docDir.value / "code",
      DocTest / unmanagedResourceDirectories += Docs.docDir.value / "code"
    )

lazy val codegen =
  project
    .in(file("slick-codegen"))
    .dependsOn(slick)
    .enablePlugins(SDLCPlugin)
    .settings(
      slickGeneralSettings,
      extTarget("codegen"),
      Docs.scaladocSettings,
      name := "Slick-CodeGen",
      description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
      scaladocSourceUrl("slick-codegen"),
      test := {}, testOnly := {}, // suppress test status output
      commonTestResourcesSetting
    )

lazy val hikaricp =
  project
    .in(file("slick-hikaricp"))
    .enablePlugins(SDLCPlugin)
    .dependsOn(slick)
    .settings(
      slickGeneralSettings,
      extTarget("hikaricp"),
      Docs.scaladocSettings,
      name := "Slick-HikariCP",
      description := "HikariCP integration for Slick (Scala Language-Integrated Connection Kit)",
      scaladocSourceUrl("slick-hikaricp"),
      test := {}, testOnly := {}, // suppress test status output
      libraryDependencies += Dependencies.hikariCP,
    )

lazy val `reactive-streams-tests` =
  project
    .dependsOn(testkit)
    .settings(
      slickGeneralSettings,
      name := "Slick-ReactiveStreamsTests",
      resolvers += Resolver.sbtPluginRepo("releases"),
      libraryDependencies += "org.scalatestplus" %% "testng-6-7" % "3.2.9.0",
      libraryDependencies ++=
        (Dependencies.logback +: Dependencies.testDBs).map(_ % Test),
      libraryDependencies += Dependencies.reactiveStreamsTCK,
      Test / parallelExecution := false,
      commonTestResourcesSetting
    )

lazy val root =
  project
    .in(file("."))
    .aggregate(slick, codegen, hikaricp, testkit)
    .enablePlugins(OrnatePlugin)
    .settings(
      slickGeneralSettings,
      extTarget("root"),
      Docs.docSettings,
      sourceDirectory := file(target.value + "/root-src"),
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      PgpKeys.publishSigned := {},
      PgpKeys.publishLocalSigned := {},
      // suppress test status output
      test := {},
      testOnly := {},
      testSampleHelloSlick := {
        Def.sequential(
          `sample-hello-slick` / Test / test,
          (`sample-hello-slick` / Compile / runMain).toTask(" HelloSlick"),
          (`sample-hello-slick` / Compile / runMain).toTask(" CaseClassMapping"),
          (`sample-hello-slick` / Compile / runMain).toTask(" QueryActions")
        ).value
      },
      testSamplePlainSql := {
        Def.sequential(
          (`sample-slick-plainsql` / Compile / runMain).toTask(" PlainSQL"),
          (`sample-slick-plainsql` / Compile / runMain).toTask(" TypedSQL")
        ).value
      },
      testSampleMultiDb := {
        Def.sequential(
          (`sample-slick-multidb` / Compile / runMain).toTask(" SimpleExample"),
          (`sample-slick-multidb` / Compile / runMain).toTask(" MultiDBExample"),
          (`sample-slick-multidb` / Compile / runMain).toTask(" MultiDBCakeExample"),
          (`sample-slick-multidb` / Compile / runMain).toTask(" CallNativeDBFunction")
        ).value
      },
      testSampleTestkit := {
        Def.sequential(
          `sample-slick-testkit-example` / Test / compile // running would require external setup
        ).value
      },
      testAllSamples := {
        Def.sequential(
          testSampleHelloSlick,
          testSamplePlainSql,
          testSampleMultiDb,
          testSampleTestkit
        ).value
      },
      testAll := {
        Def.sequential(
          testkit / Test / test,
          testkit / DocTest / test,
          `reactive-streams-tests` / Test / test,
          slick / Compile / packageDoc,
          codegen / Compile / packageDoc,
          hikaricp / Compile / packageDoc,
          testkit / Compile / packageDoc,
          versionSchemeEnforcerCheck
        ).value
      },
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 12)) => libraryDependencies.value
          case _             => libraryDependencies.value.filter(!_.configurations.contains(Ornate.name))
        }
      }
    )

// sample projects under ./samples
lazy val `sample-hello-slick` =
  project
    .in(file("samples/hello-slick"))
    .settings(sampleSettings)
    .dependsOn(slick)

lazy val `sample-slick-multidb` =
  project
    .in(file("samples/slick-multidb"))
    .settings(sampleSettings)
    .dependsOn(slick)

lazy val `sample-slick-plainsql` =
  project
    .in(file("samples/slick-plainsql"))
    .settings(sampleSettings)
    .dependsOn(slick)

lazy val `sample-slick-testkit-example` =
  project
    .in(file("samples/slick-testkit-example"))
    .settings(sampleSettings)
    .dependsOn(slick, testkit % "test")

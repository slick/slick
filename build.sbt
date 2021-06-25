import Docs.docDir
import com.jsuereth.sbtpgp.PgpKeys
import com.typesafe.tools.mima.core._


val binaryCompatSlickVersion =
  settingKey[Option[String]]("The slick version this build should be compatible with, if any")

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

lazy val sampleOverridden = AttributeKey[Boolean]("sample-settings-overridden")
lazy val updateSampleCommand = Command.command("update-sample")(updateSampleSettings)

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

def sampleSettingsOverride = Seq(
  crossScalaVersions := (LocalProject("slick") / crossScalaVersions).value,

  scalaVersion := (LocalProject("slick") / scalaVersion).value,

  libraryDependencies := libraryDependencies.value.map { m =>
    if (m.organization != (LocalProject("slick") / organization).value) m
    else m.withRevision((ThisBuild / version).value)
  },

  Compile / unmanagedClasspath :=
    Attributed.blank(baseDirectory.value.getParentFile / "resources") +: (Compile / unmanagedClasspath).value,
)

def sampleProject(s: String): Project = Project(id = "sample-"+s, base = file("samples/"+s)).settings(
  commands += updateSampleCommand
)


def updateSampleSettings(state: State): State = {

  if(state.get(sampleOverridden) getOrElse false) {
    state
  } else {
    val nst = state.put(sampleOverridden, true)
    val extracted = Project.extract(nst)

    extracted.appendWithSession(
      sampleSettingsOverride,
      nst
    ).put(sampleOverridden, false)
  }
}


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
    libraryDependencies ++= (if (scalaVersion.value.startsWith("2."))
      Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % config,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % config)
      else
        Seq("org.scala-lang" % "scala3-compiler_3" % scalaVersion.value % config))
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
)

ThisBuild / version := "3.4.0-SNAPSHOT"

ThisBuild / crossScalaVersions := Dependencies.scalaVersions
ThisBuild / scalaVersion := Dependencies.scalaVersions.last

// Slick base version for binary compatibility checks.
// The next release to be cut from master will be 3.4.0 during develop of 3.4.0 we check compatibility with 3.3.0.
// The goal is not to stop any breaking change, but to make us aware.
// For each breaking change we should add MiMa exclusions.
// This will also help us decide when a PR can be backported in 3.3.x branch.
ThisBuild / binaryCompatSlickVersion := None

ThisBuild / docDir := (root / baseDirectory).value / "doc"


lazy val slick =
  project
    .enablePlugins(SDLCPlugin, MimaPlugin)
    .settings(
      slickGeneralSettings,
      FMPP.preprocessorSettings,
      extTarget("slick"),
      Docs.scaladocSettings,

      name := "Slick",
      description := "Scala Language-Integrated Connection Kit",
      libraryDependencies ++= Dependencies.mainDependencies,
      libraryDependencies ++= (if (scalaVersion.value.startsWith("2.")) Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value) else Nil),
      scaladocSourceUrl("slick"),
      Compile / doc / scalacOptions ++= Seq(
        "-doc-root-content", "scaladoc-root.txt"
      ),

      // suppress test status output
      test := {},
      testOnly := {},

      mimaPreviousArtifacts :=
        binaryCompatSlickVersion.value.toSet
          .map((v: String) => "com.typesafe.slick" % ("slick_" + scalaBinaryVersion.value) % v),

      mimaBinaryIssueFilters ++= Seq(
        ProblemFilters.exclude[MissingClassProblem]("slick.util.MacroSupportInterpolationImpl$"),
        ProblemFilters.exclude[MissingClassProblem]("slick.util.MacroSupportInterpolationImpl"),
        // #1997 added new method ColumnExtensionMethods.in
        ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.in"),
        // #1958 changes for scala 2.13 support (Iterable return type instead of Traversable)
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.util.ConstArray.from"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.util.SQLBuilder.sep"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.BaseColumnExtensionMethods.inSetBind"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.BaseColumnExtensionMethods.inSet"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.ColumnExtensionMethods.inSetBind"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.ColumnExtensionMethods.inSet"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.OptionColumnExtensionMethods.inSetBind"),
        ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.OptionColumnExtensionMethods.inSet"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSetBind"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSet"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSetBind"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSet"),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.in"),
        // #2025 default parameters for AsyncExecutor.apply have been removed and replaced by overloads
        ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$5"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$6"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$7"),
        // #2221 Removing unused method for 3.3.x -> 3.4.x or 4.0.0 release
        ProblemFilters.exclude[DirectMissingMethodProblem](
          "slick.jdbc.MySQLProfile#UpsertBuilder.buildInsertIgnoreStart"
        )
      ),


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

lazy val `reactive-streams-test` =
  project
    .dependsOn(testkit)
    .settings(
      slickGeneralSettings,
      name := "Slick-ReactiveStreamsTests",
      resolvers += Resolver.sbtPluginRepo("releases"),
      libraryDependencies += "org.scalatestplus" %% "testng-6-7" % "3.2.6.0",
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
      commands ++= Seq(
        Command.command("updateSampleHelloSlick")(updateSampleHelloSlick),
        Command.command("updateSampleSlickPlainsql")(updateSampleSlickPlainsql),
        Command.command("updateSampleSlickMultidb")(updateSampleSlickMultidb),
        Command.command("updateSampleSlickTestkitExample")(updateSampleSlickTestkitExample),
      ),
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
          `reactive-streams-test` / Test / test,
          slick / Compile / packageDoc,
          codegen / Compile / packageDoc,
          hikaricp / Compile / packageDoc,
          testkit / Compile / packageDoc,
          slick / Compile / mimaReportBinaryIssues // enable for minor versions
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
    .settings(commands += updateSampleCommand)
    .dependsOn(slick)

lazy val `sample-slick-multidb` =
  project
    .in(file("samples/slick-multidb"))
    .settings(sampleSettings)
    .settings(commands += updateSampleCommand)
    .dependsOn(slick)

lazy val `sample-slick-plainsql` =
  project
    .in(file("samples/slick-plainsql"))
    .settings(sampleSettings)
    .settings(commands += updateSampleCommand)

    .dependsOn(slick)

lazy val `sample-slick-testkit-example` =
  project
    .in(file("samples/slick-testkit-example"))
    .settings(sampleSettings)
    .settings(commands += updateSampleCommand)
    .dependsOn(slick, testkit % "test")

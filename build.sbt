import com.jsuereth.sbtpgp.PgpKeys
import com.typesafe.tools.mima.core.{DirectMissingMethodProblem, IncompatibleMethTypeProblem, MissingClassProblem, ProblemFilters, ReversedMissingMethodProblem}


val binaryCompatSlickVersion =
  settingKey[Option[String]]("The slick version this build should be compatible with, if any")

val testAll = taskKey[Unit]("Run all tests")

val cleanCompileTimeTests =
  taskKey[Unit]("Delete files used for compile-time tests which should be recompiled every time.")

val repoKind = settingKey[String]("""Maven repository kind ("snapshots" or "releases")""")

/* Test Configuration for running tests on doc sources */
val DocTest = config("doctest").extend(Test)

val tagTestGroupOther = Tags.Tag("test-group-other")
Global / concurrentRestrictions :=
  List(
    Tags.limit(Tags.ForkedTestGroup, 4),
    Tags.limit(tagTestGroupOther, 1),
    Tags.limit(Tags.Tag("test-group-DB2"), 1),
    Tags.limit(Tags.Tag("test-group-Postgres"), 1),
    Tags.exclusiveGroup(tagTestGroupOther),
    Tags.exclusiveGroup(Tags.Clean))



def scaladocSourceUrl(dir: String) =
  Compile / doc / scalacOptions ++= {
    val ref = Versioning.currentRef(baseDirectory.value)
    Seq(
      "-doc-source-url",
      s"https://github.com/slick/slick/blob/$ref/$dir/src/main€{FILE_PATH}.scala"
    )
  }



inThisBuild(
  Seq(
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
    homepage := Some(url("https://scala-slick.org")),
    startYear := Some(2008),
    licenses += ("Two-clause BSD-style license", url("https://github.com/slick/slick/blob/main/LICENSE.txt")),
    developers :=
      List(
        Developer("szeiger", "Stefan Zeiger", "", url("http://szeiger.de")),
        Developer("hvesalai", "Heikki Vesalainen", "", url("https://github.com/hvesalai/"))
      ),
    scmInfo := Some(ScmInfo(url("https://github.com/slick/slick"), "scm:git:git@github.com:slick/slick.git")),
    scalacOptions ++=
      List(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xsource:3",
        "-Wconf:cat=unused-imports&src=src_managed/.*:silent"
      )
  )
)

def slickGeneralSettings =
  Seq(
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= {
      _.withConfigurations(Vector(Compile, Runtime, Optional))
    },
    sonatypeProfileName := "com.typesafe.slick",
    Compile / doc / scalacOptions ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (Compile / sourceDirectory).value.getPath, // make scaladoc strip location of linked source path
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    ),
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


ThisBuild / version := "3.4.0-SNAPSHOT"

ThisBuild / crossScalaVersions := Dependencies.scalaVersions
ThisBuild / scalaVersion := Dependencies.scalaVersions.last

// Slick base version for binary compatibility checks.
// The next release to be cut from master will be 3.4.0 during develop of 3.4.0 we check compatibility with 3.3.0.
// The goal is not to stop any breaking change, but to make us aware.
// For each breaking change we should add MiMa exclusions.
// This will also help us decide when a PR can be backported in 3.3.x branch.
ThisBuild / binaryCompatSlickVersion := None

//ThisBuild / docDir := (root / baseDirectory).value / "doc"


ThisBuild / binaryCompatSlickVersion  := Some("3.3.0")

ThisBuild / versionScheme := Some("pvp")

ThisBuild / versionPolicyIntention := Versioning.BumpMajor

ThisBuild / versionPolicyIgnoredInternalDependencyVersions := Some("^\\d+\\.\\d+\\.\\d+-pre\\.\\d+\\.\\w+\\.dirty".r)

val buildCapabilitiesTable = taskKey[File]("Build the capabilities.csv table for the documentation")

val buildCompatReport = taskKey[File]("Build the compatibility report")

val writeVersionToGitHubOutput = taskKey[Unit]("Write the version to github output")
val writeCompatReportToGitHubOutput = taskKey[Unit]("Write the compatibility report to github output")

val docDir = settingKey[File]("Base directory for documentation")

ThisBuild / docDir := (site / baseDirectory).value


lazy val slick =
  project
    .enablePlugins( MimaPlugin)
    .settings(
      slickGeneralSettings,
      compilerDependencySetting("provided"),
      FMPP.preprocessorSettings,
      extTarget("slick"),
//      Docs.scaladocSettings,

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
      mimaCheckDirection := "both",

      mimaPreviousArtifacts :=
        (ThisBuild / binaryCompatSlickVersion).value.toSet
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
    .dependsOn(slick, codegen % s"compile->compile;${TypeProviders.TypeProvidersConfig.name}->test", hikaricp)
    .settings(
      slickGeneralSettings,
      compilerDependencySetting(Provided.name),
      compilerDependencySetting(TypeProviders.TypeProvidersConfig.name),
      inConfig(DocTest)(Defaults.testSettings),
//      Docs.scaladocSettings,
      TypeProviders.codegenSettings,
      extTarget("testkit"),
      name := "Slick-TestKit",
      description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
      scaladocSourceUrl("slick-testkit"),
      testOptions +=
        Tests.Argument(
          Some(TestFrameworks.JUnit),
          List("-q", "-v", "-s", "-a", "-Djava.awt.headless=true") ++
            (if (sys.env.get("GITHUB_ACTIONS").contains("true"))
              Some("--run-listener=com.typesafe.slick.testkit.util.GitHubActionsRunListener")
            else
              None)
        ), //scalacOptions in Compile += "-Yreify-copypaste",
      libraryDependencies ++=
        Dependencies.junit ++:
          (Dependencies.reactiveStreamsTCK % Test) +:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % Test) ++:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % TypeProviders.TypeProvidersConfig),
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
      Test / testGrouping := {
        val re = """slick\.test\.profile\.(.+?)(?:\d\d+)?(?:Disk|Mem|Rownum|SQLJDBC)?Test$""".r
        (Test / definedTests).value
          .groupBy(_.name match {
            case re(name) => name
            case _ => "other"
          })
          .map { case (name, tests) =>
            new Tests.Group(name, tests, Tests.SubProcess(ForkOptions()), Seq(Tags.Tag(s"test-group-$name") -> 1))
          }
          .toSeq
          .sortBy(_.name)
      },
      buildCapabilitiesTable := {
        val logger = ConsoleLogger()
        val file = (buildCapabilitiesTable / sourceManaged).value / "capabilities.md"
        Run.run(
          mainClass = "com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
          classpath = (Compile / fullClasspath).value.map(_.data),
          options = Seq(file.toString),
          log = logger
        )(runner.value)
          .map(_ => file)
          .get
      },
      DocTest / unmanagedSourceDirectories += docDir.value / "code",
      DocTest / unmanagedResourceDirectories += docDir.value / "code",
      DocTest / parallelExecution := false
    )

lazy val codegen =
  project
    .in(file("slick-codegen"))
    .dependsOn(slick)
    .settings(
      slickGeneralSettings,
      extTarget("codegen"),
//      Docs.scaladocSettings,
      name := "Slick-CodeGen",
      description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
      scaladocSourceUrl("slick-codegen"),
      test := {}, testOnly := {}, // suppress test status output
      commonTestResourcesSetting
    )

lazy val hikaricp =
  project
    .in(file("slick-hikaricp"))
    .dependsOn(slick)
    .settings(
      slickGeneralSettings,
      extTarget("hikaricp"),
//      Docs.scaladocSettings,
      name := "Slick-HikariCP",
      description := "HikariCP integration for Slick (Scala Language-Integrated Connection Kit)",
      scaladocSourceUrl("slick-hikaricp"),
      test := {}, testOnly := {}, // suppress test status output
      libraryDependencies += Dependencies.hikariCP.exclude("org.slf4j", "*"),
    )

//lazy val `reactive-streams-tests` =
//  project
//    .in(file("reactive-streams-tests"))
//    .dependsOn(testkit)
//    .settings(
//      slickGeneralSettings,
//      name := "Slick-ReactiveStreamsTests",
//      resolvers += Resolver.sbtPluginRepo("releases"),
//      libraryDependencies += "org.scalatestplus" %% "testng-7-5" % "3.2.16.0",
//      libraryDependencies ++=
//        (Dependencies.logback +: Dependencies.testDBs).map(_ % Test),
//      libraryDependencies += Dependencies.reactiveStreamsTCK,
//      Test / parallelExecution := false,
//      commonTestResourcesSetting
//    )


def writeToGitHubOutput(text: String, logger: Logger) = {
  val string = s"result=$text\n"
  logger.info(s"Writing to GitHub Actions output file: $string")
  IO.append(file(sys.env("GITHUB_OUTPUT")), string)
}

lazy val site: Project =
  project
    .in(file("doc"))
    .enablePlugins(Docs)
    .settings(
      description := "Scala Slick documentation",
      scaladocDirs := Seq(
        "api" -> (slick / Compile / doc).value,
        "codegen-api" -> (codegen / Compile / doc).value,
        "hikaricp-api" -> (hikaricp / Compile / doc).value,
        "testkit-api" -> (testkit / Compile / doc).value
      ),
      buildCompatReport := {
        val compatReports =
          (CompatReport / compatReportMarkdown)
            .all(ScopeFilter(inProjects(slick, codegen, hikaricp, testkit)))
            .value
        val file = (buildCompatReport / target).value / "compat-report.md"
        IO.write(
          file,
          if (compatReports.forall(_.trim.isEmpty))
            "There are no incompatible changes"
          else
            compatReports.mkString(
              "## Incompatible changes\n\n",
              "\n\n",
              "\n"
            )
        )
        file
      },
      writeVersionToGitHubOutput := {
        writeToGitHubOutput(version.value, streams.value.log)
      },
      writeCompatReportToGitHubOutput := {
        writeToGitHubOutput(buildCompatReport.value.toString, streams.value.log)
      },
      preprocessDocs := {
        val out = (preprocessDocs / target).value

        val capabilitiesTableFile = (testkit / buildCapabilitiesTable).value
        IO.copyFile(capabilitiesTableFile, out / capabilitiesTableFile.getName)

        val compatReportFile = buildCompatReport.value
        IO.copyFile(compatReportFile, out / compatReportFile.getName)

        preprocessDocs.value
      },
      (Compile / paradoxMarkdownToHtml / excludeFilter) :=
        (Compile / paradoxMarkdownToHtml / excludeFilter).value ||
          globFilter("capabilities.md"),
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      test := {},
      testOnly := {},
      versionPolicyPreviousVersions := Nil
    )


lazy val root =
  project
    .in(file("."))
    .aggregate(slick, codegen, hikaricp, testkit, site)
//    .enablePlugins(OrnatePlugin)
    .settings(
      slickGeneralSettings,
      extTarget("root"),
//      Docs.docSettings,
      sourceDirectory := file(target.value + "/root-src"),
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      versionPolicyPreviousVersions := Nil,
      PgpKeys.publishSigned := {},
      PgpKeys.publishLocalSigned := {},
      // suppress test status output
      test := {},
      testOnly := {},
      testAll := {
        Def.sequential(
          testkit / Test / test,
          testkit / DocTest / test,
//          `reactive-streams-tests` / Test / test,
          slick / Compile / packageDoc,
          codegen / Compile / packageDoc,
          hikaricp / Compile / packageDoc,
          testkit / Compile / packageDoc,
          slick / versionPolicyCheck,
          testkit / versionPolicyCheck,
          hikaricp / versionPolicyCheck,
          codegen / versionPolicyCheck,
          slick / Compile / mimaReportBinaryIssues // enable for minor versions
        ).value
      },
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 12)) => libraryDependencies.value
          case _             => libraryDependencies.value
        }
      }
    )

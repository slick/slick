import sbt._
import sbt.nio.Keys.fileTreeView
import Keys._
import com.typesafe.tools.mima.core.{DirectMissingMethodProblem, IncompatibleMethTypeProblem, MissingClassProblem, ProblemFilters, ReversedMissingMethodProblem}
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaBinaryIssueFilters, mimaPreviousArtifacts}
import com.jsuereth.sbtpgp.PgpKeys

object Settings {
//  val slickVersion =

  val testSamples = taskKey[Unit]("Run tests in the sample apps")
  val testSample1 = taskKey[Unit]("Run tests in the sample app")
  val testSample2 = taskKey[Unit]("Run tests in the sample app")
  val testSample3 = taskKey[Unit]("Run tests in the sample app")
  val testSample4 = taskKey[Unit]("Run tests in the sample app")

  lazy val sampleOverridden = AttributeKey[Boolean]("sample-settings-overridden")
  lazy val updateSampleCommand = Command.command("update-sample")(updateSampleSettings)

  val cleanCompileTimeTests = taskKey[Unit]("Delete files used for compile-time tests which should be recompiled every time.")

  val repoKind = settingKey[String]("""Maven repository kind ("snapshots" or "releases")""")

  val binaryCompatSlickVersion = settingKey[Option[String]]("The slick version this build should be compatible with, if any")

  /* Test Configuration for running tests on doc sources */
  val DocTest = config("doctest") extend(Test)
  val CompileConfig = config("compile")
  val TestConfig = config("test")

  def slickProjectSettings = (
    slickGeneralSettings ++
      FMPP.preprocessorSettings ++
      extTarget("slick") ++
      Docs.scaladocSettings ++
      Seq(
        name := "Slick",
        description := "Scala Language-Integrated Connection Kit",
        libraryDependencies ++= Dependencies.mainDependencies,
        Compile / doc / scalacOptions ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
          "-doc-root-content", "scaladoc-root.txt"
        ),
        test := {}, testOnly :=  {}, // suppress test status output
        mimaPreviousArtifacts := binaryCompatSlickVersion.value.toSet.map { v: String =>
          "com.typesafe.slick" % ("slick_" + scalaBinaryVersion.value) % v
        },
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
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$7")
        ),
        libraryDependencies ++= {
          if(!scalaVersion.value.startsWith("2.")) Nil
          else Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
        }
      )
  )

  def slickTestkitProjectSettings = (
    slickGeneralSettings ++
      compilerDependencySetting("provided") ++
      inConfig(DocTest)(Defaults.testSettings) ++
      Docs.scaladocSettings ++
      TypeProviders.codegenSettings ++
      extTarget("testkit") ++
      Seq(
        name := "Slick-TestKit",
        description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
        Compile / doc / scalacOptions ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-testkit/src/main€{FILE_PATH}.scala"
        ),
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
        //javaOptions in run += "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15072.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
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
          Run.run( "com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
                   (Compile / fullClasspath).value.map(_.data),
                   Seq(Docs.docDir.value / "capabilities.md") map (_.toString),
                   logger)(runner.value)
        },
        DocTest / unmanagedSourceDirectories += Docs.docDir.value / "code",
        DocTest / unmanagedResourceDirectories += Docs.docDir.value / "code"
      )
  )

  def slickCodegenProjectSettings = (
    slickGeneralSettings ++
      extTarget("codegen") ++
      Docs.scaladocSettings ++
      Seq(
        name := "Slick-CodeGen",
        description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
        Compile / doc / scalacOptions ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-codegen/src/main€{FILE_PATH}.scala"
        ),
        test := {}, testOnly := {}, // suppress test status output
        commonTestResourcesSetting
      )
  )

  def slickHikariCPProjectSettings = (
    slickGeneralSettings ++
      extTarget("hikaricp") ++
      Docs.scaladocSettings ++
      Seq(
        name := "Slick-HikariCP",
        description := "HikariCP integration for Slick (Scala Language-Integrated Connection Kit)",
        Compile / doc / scalacOptions ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-hikaricp/src/main€{FILE_PATH}.scala"
        ),
        test := {}, testOnly := {}, // suppress test status output
        libraryDependencies += Dependencies.hikariCP,
      )
  )

  def aRootProjectSettings = (
    slickGeneralSettings ++
      extTarget("root") ++
      Docs.docSettings ++
      Seq(
        sourceDirectory := file(target.value + "/root-src"),
        publishArtifact := false,
        publish := {},
        publishLocal := {},
        PgpKeys.publishSigned := {},
        PgpKeys.publishLocalSigned := {},
        test := {}, testOnly := {} // suppress test status output
      )
  )

  def reactiveStreamsTestProjectSettings = (
    slickGeneralSettings ++ Seq(
      name := "Slick-ReactiveStreamsTests",
      resolvers += Resolver.sbtPluginRepo("releases"),
      libraryDependencies += "org.scalatestplus" %% "testng-6-7" % "3.2.6.0",
      libraryDependencies ++=
        (Dependencies.logback +: Dependencies.testDBs).map(_ % Test),
      libraryDependencies += Dependencies.reactiveStreamsTCK,
      Test / parallelExecution := false,
      commonTestResourcesSetting
    )
  )

  def slickGeneralSettings =
    slickPublishSettings ++ slickScalacSettings ++ slickScalaSettings ++ Seq(
      logBuffered := false
    )

  def commonTestResourcesSetting = (
    Test / unmanagedResourceDirectories +=
      (LocalProject("root") / baseDirectory).value / "common-test-resources"
  )

  def slickScalaSettings = {
    sys.props("scala.home.local") match {
      case null => publishedScalaSettings
      case path =>
        scala.Console.err.println("Using local scala at " + path)
        localScalaSettings(path)
    }
  }

  def slickPublishSettings = Seq(
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    repoKind := (if (version.value.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"),
    publishTo := (
      repoKind.value match {
        case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
        case "releases" =>  Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
      }
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= { _.withConfigurations(Vector(Compile, Runtime, Optional)) },
    homepage := Some(url("http://slick.typesafe.com")),
    startYear := Some(2008),
    licenses += ("Two-clause BSD-style license", url("http://github.com/slick/slick/blob/master/LICENSE.txt")),
    pomExtra := pomExtraXml
  )

  def pomExtraXml = (
    <developers>
      <developer>
        <id>szeiger</id>
        <name>Stefan Zeiger</name>
        <timezone>+1</timezone>
        <url>http://szeiger.de</url>
      </developer>
      <developer>
        <id>hvesalai</id>
        <name>Heikki Vesalainen</name>
        <timezone>+2</timezone>
        <url>https://github.com/hvesalai/</url>
      </developer>
    </developers>
    <scm>
      <url>git@github.com:slick/slick.git</url>
      <connection>scm:git:git@github.com:slick/slick.git</connection>
    </scm>
  )

  def slickScalacSettings = Seq(
    scalacOptions ++= List("-deprecation", "-feature", "-unchecked", "-language:implicitConversions"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 =>
          Seq("-Xfuture")
        case _ =>
          Nil
      }
    },
    Compile / doc / scalacOptions ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (Compile / sourceDirectory).value.getPath, // needed for scaladoc to strip the location of the linked source path
      "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    )
  )

  // set the scala-compiler dependency unless a local scala is in use
  def compilerDependencySetting(config: String) =
    libraryDependencies ++= {
      if (sys.props("scala.home.local") != null || !scalaVersion.value.startsWith("2.")) Nil
      else Seq("org.scala-lang" % "scala3-compiler_3" % scalaVersion.value % config)
    }

  def publishedScalaSettings = Seq(
    scalaVersion := Dependencies.scalaVersions.tail.head,
    crossScalaVersions := Dependencies.scalaVersions
  )

  def localScalaSettings(path: String): Seq[Setting[_]] = Seq(
    scalaVersion := "2.10.0-unknown",
    scalaBinaryVersion := "2.10.0-unknown",
    crossVersion := CrossVersion.disabled,
    scalaHome := Some(file(path)),
    autoScalaLibrary := false,
    // When using scala.home.local property adds all jars from <SCALA_HOME>/lib directory.
    unmanagedJars := Attributed.blankSeq(scalaInstance.value.allJars.toSeq),
    Compile / unmanagedJars := Attributed.blankSeq(scalaInstance.value.allJars.toSeq),
    Test / unmanagedJars := Attributed.blankSeq(scalaInstance.value.allJars.toSeq),
  )

  def sampleProject(s: String): Project = Project(id = "sample-"+s, base = file("samples/"+s)).settings(
    commands += updateSampleCommand
  )

  def extTarget(extName: String): Seq[Setting[File]] = {
    sys.props("slick.build.target") match {
      case null => Seq.empty
      case path => Seq(target := file(path + "/" + extName))
    }
  }

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

  // Override settings of sample projects when they are used as subprojects of the main Slick build.
  //
  // The sample projects are packaged as standalone sbt projects via the Example Code Service
  // (https://example.lightbend.com) so they have to be complete and usable on their own and they must
  // not contain any unnecessary code that is only needed when they are run as part of the main build.
  // The settings here (which are loaded after build.sbt) ensure that the Scala version and the Slick
  // version match the ones of the main build.
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
}

import sbt._
import Keys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.core.{ProblemFilters, MissingClassProblem}
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaBinaryIssueFilters, mimaReportBinaryIssues}
import com.typesafe.sbt.osgi.SbtOsgi.{osgiSettings, OsgiKeys}
import com.typesafe.sbt.pgp.PgpKeys

object Settings {
//  val slickVersion =

  val testSamples = TaskKey[Unit]("testSamples", "Run tests in the sample apps")

  val repoKind =
    SettingKey[String]("repo-kind",
                       """"Maven repository kind ("snapshots" or "releases")""")

  val binaryCompatSlickVersion = SettingKey[Option[String]]("binaryCompatSlickVersion",
                                                            "The slick version this build should be compatible with, if any")

  /* Test Configuration for running tests on doc sources */
  def DocTest = config("doctest") extend(Test)

  def slickProjectSettings = (
    slickGeneralSettings ++
      compilerDependencySetting("macro") ++
      inConfig(config("macro"))(Defaults.configSettings) ++
      FMPP.preprocessorSettings ++
      mimaDefaultSettings ++
      extTarget("slick") ++
      Docs.scaladocSettings ++
      osgiSettings ++
      Seq(
        name := "Slick",
        description := "Scala Language-Integrated Connection Kit",
        libraryDependencies ++= Dependencies.mainDependencies,
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
          "-doc-root-content", "scaladoc-root.txt"
        ),
        test := (), testOnly :=  (), // suppress test status output
        mimaPreviousArtifacts := binaryCompatSlickVersion.value.toSet.map { v: String =>
          "com.typesafe.slick" % ("slick_" + scalaBinaryVersion.value) % v
        },
        mimaBinaryIssueFilters ++= Seq(
          ProblemFilters.exclude[MissingClassProblem]("slick.util.MacroSupportInterpolationImpl$"),
          ProblemFilters.exclude[MissingClassProblem]("slick.util.MacroSupportInterpolationImpl")
        ),
        ivyConfigurations += config("macro").hide.extend(Compile),
        unmanagedClasspath in Compile ++= (products in config("macro")).value,
        libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        mappings in (Compile, packageSrc) ++= (mappings in (config("macro"), packageSrc)).value,
        mappings in (Compile, packageBin) ++= (mappings in (config("macro"), packageBin)).value,
        OsgiKeys.exportPackage := Seq("slick", "slick.*", "scala.slick", "scala.slick.*"),
        OsgiKeys.importPackage := Seq(Osgi.osgiImport("scala*", scalaVersion.value), "*"),
        OsgiKeys.privatePackage := Nil
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
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-testkit/src/main€{FILE_PATH}.scala"
        ),
        testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a", "-Djava.awt.headless=true"),
        //scalacOptions in Compile += "-Yreify-copypaste",
        libraryDependencies ++=
          Dependencies.junit ++:
          (Dependencies.reactiveStreamsTCK % "test") +:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % "test") ++:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % "codegen"),
        parallelExecution in Test := false,
        fork in run := true,
        //connectInput in run := true,
        //javaOptions in run += "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15072.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
        javaOptions in run += "-Dslick.ansiDump=true",
        //javaOptions in run += "-verbose:gc",
        compile in Test ~= { a =>
          // Delete classes in "compile" packages after compiling. (Currently only slick.test.compile.NestedShapeTest)
          // These are used for compile-time tests and should be recompiled every time.
          val products = a.relations.allProducts.toSeq ** new SimpleFileFilter(_.getParentFile.getName == "compile")
          IO.delete(products.get)
          a
        },
        Docs.buildCapabilitiesTable := {
          val logger = ConsoleLogger()
          Run.run( "com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
                   (fullClasspath in Compile).value.map(_.data),
                   Seq(Docs.docDir.value / "capabilities.md") map (_.toString),
                   logger)(runner.value)
        },
        unmanagedSourceDirectories in DocTest += Docs.docDir.value / "code",
        unmanagedResourceDirectories in DocTest += Docs.docDir.value / "code"
      )
  )

  def slickCodegenProjectSettings = (
    slickGeneralSettings ++
      extTarget("codegen") ++
      Docs.scaladocSettings ++
      Seq(
        name := "Slick-CodeGen",
        description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-codegen/src/main€{FILE_PATH}.scala"
        ),
        test := (), testOnly := (), // suppress test status output
        commonTestResourcesSetting
      )
  )

  def slickHikariCPProjectSettings = (
    slickGeneralSettings ++
      extTarget("hikaricp") ++
      Docs.scaladocSettings ++
      osgiSettings ++
      Seq(
        name := "Slick-HikariCP",
        description := "HikariCP integration for Slick (Scala Language-Integrated Connection Kit)",
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-hikaricp/src/main€{FILE_PATH}.scala"
        ),
        test := (), testOnly := (), // suppress test status output
        libraryDependencies += Dependencies.hikariCP,
        OsgiKeys.exportPackage := Seq("slick.jdbc.hikaricp"),
        OsgiKeys.importPackage := Seq(
          Osgi.osgiImport("slick*", (version in ThisBuild).value),
          Osgi.osgiImport("scala*", scalaVersion.value),
          "*"
        ),
        OsgiKeys.privatePackage := Nil
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
        test := (), testOnly := () // suppress test status output
      )
  )

  def reactiveStreamsTestProjectSettings = (
    slickGeneralSettings ++ Seq(
      name := "Slick-ReactiveStreamsTests",
      resolvers += Resolver.sbtPluginRepo("releases"),
      libraryDependencies += Dependencies.scalaTestFor(scalaVersion.value),
      libraryDependencies ++=
        (Dependencies.logback +: Dependencies.testDBs).map(_ % "test"),
      libraryDependencies += Dependencies.reactiveStreamsTCK,
      parallelExecution in Test := false,
      commonTestResourcesSetting
    )
  )

  def osgiTestProjectSettings = slickGeneralSettings ++ Seq(
    name := "Slick-OsgiTests",
    libraryDependencies ++= (
      Dependencies.h2 +: Dependencies.logback +: Dependencies.reactiveStreams +:
        Dependencies.junit ++: Dependencies.paxExam
    ).map(_ % "test"),
    fork in Test := true,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
    javaOptions in Test ++= Seq(
      // Use '@' as a seperator that shouldn't appear in any filepaths or names
      "-Dslick.osgi.bundlepath=" + Osgi.osgiBundleFiles.value.map(_.getCanonicalPath).mkString("@"),
      "-Dorg.ops4j.pax.logging.DefaultServiceLog.level=WARN"
    ),
    Osgi.osgiBundleFiles := Seq((OsgiKeys.bundle in LocalProject("slick")).value),
    Osgi.osgiBundleFiles ++=
      (dependencyClasspath in Compile in LocalProject("slick")).value.
      map(_.data).filterNot(_.isDirectory),
    Osgi.osgiBundleFiles ++=
      (dependencyClasspath in Test).value.map(_.data).
      filter(f => f.name.contains("logback-") || f.name.contains("h2")),
    publishArtifact := false,
    commonTestResourcesSetting
  )

  def slickGeneralSettings =
    slickPublishSettings ++ slickScalacSettings ++ slickScalaSettings ++ crossScalaFoldersSettings ++ Seq(
      logBuffered := false
    )

  def commonTestResourcesSetting = (
    unmanagedResourceDirectories in Test +=
      (baseDirectory in LocalProject("root")).value / "common-test-resources"
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
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime, Optional))) },
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
    scalacOptions ++= List("-deprecation", "-feature", "-unchecked"),
    // -Xfuture is deprecated: Not used since 2.13.
    scalacOptions ++= {
      if (scalaVersion.value.startsWith("2.13")) List.empty
      else List("-Xfuture")
    },
    scalacOptions in (Compile, doc) ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (sourceDirectory in Compile).value.getPath, // needed for scaladoc to strip the location of the linked source path
      "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    )
  )

  // set the scala-compiler dependency unless a local scala is in use
  def compilerDependencySetting(config: String) =
    if (sys.props("scala.home.local") != null) Nil else Seq(
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % config
    )

  def publishedScalaSettings = Seq(
    scalaVersion := Dependencies.scalaVersions.head,
    crossScalaVersions := Dependencies.scalaVersions
  )

  def localScalaSettings(path: String): Seq[Setting[_]] = Seq(
    scalaVersion := "2.10.0-unknown",
    scalaBinaryVersion := "2.10.0-unknown",
    crossVersion := CrossVersion.Disabled,
    scalaHome := Some(file(path)),
    autoScalaLibrary := false,
    unmanagedJars := scalaInstance.map( _.jars.classpath).value,
    unmanagedJars in config("compile") := scalaInstance.map( _.jars.classpath).value,
    unmanagedJars in config("test") := scalaInstance.map( _.jars.classpath).value,
    unmanagedJars in config("macro") := scalaInstance.map( _.jars.classpath).value
  )

  def crossScalaFoldersSettings: Seq[Setting[_]] = Seq(
    unmanagedSourceDirectories in Compile += {
      val sourceDir = (sourceDirectory in Compile).value
      val scalaVer = scalaVersion.value
      if (scalaVer.startsWith("2.13")) {
        sourceDir / "scala-2.13+"
      } else {
        sourceDir / "scala-2.13-"
      }
    }
  )

  def sampleProject(s: String): Project = Project(id = "sample-"+s, base = file("samples/"+s))
    .addSbtFiles(file("../override.sbt"))

  def extTarget(extName: String): Seq[Setting[File]] = {
    sys.props("slick.build.target") match {
      case null => Seq.empty
      case path => Seq(target := file(path + "/" + extName))
    }
  }

}

import sbt._
import Keys._
import Tests._
import com.typesafe.sbt.site.SphinxSupport.{Sphinx, sphinxEnv, sphinxProperties}
import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.SbtSite.SiteKeys.makeSite
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{previousArtifact, binaryIssueFilters}
import com.typesafe.tools.mima.core.{ProblemFilters, MissingClassProblem}
import com.typesafe.sbt.osgi.SbtOsgi.{osgiSettings, OsgiKeys}
import com.typesafe.sbt.sdlc.Plugin._

object SlickBuild extends Build {

  val slickVersion = "2.2.0-SNAPSHOT"
  val binaryCompatSlickVersion = "2.2.0" // Slick base version for binary compatibility checks
  val scalaVersions = Seq("2.10.4", "2.11.2")

  /** Dependencies for reuse in different parts of the build */
  object Dependencies {
    val junit = Seq(
      "junit" % "junit-dep" % "4.10",
      "com.novocode" % "junit-interface" % "0.10-M4"
    )
    val slf4j = "org.slf4j" % "slf4j-api" % "1.6.4"
    val logback = "ch.qos.logback" % "logback-classic" % "0.9.28"
    val typesafeConfig = "com.typesafe" % "config" % "1.2.1"
    val mainDependencies = Seq(slf4j, typesafeConfig)
    val h2 = "com.h2database" % "h2" % "1.3.170"
    val testDBs = Seq(
      h2,
      "org.xerial" % "sqlite-jdbc" % "3.7.2",
      "org.apache.derby" % "derby" % "10.9.1.0",
      "org.hsqldb" % "hsqldb" % "2.2.8"
    )
    val paxExamVersion = "2.6.0"
    val paxExam = Seq(
      "org.ops4j.pax.exam"     % "pax-exam-container-native"  % paxExamVersion,
      "org.ops4j.pax.exam"     % "pax-exam-junit4"            % paxExamVersion,
      "org.ops4j.pax.exam"     % "pax-exam-link-assembly"     % paxExamVersion,
      "org.ops4j.pax.url"      % "pax-url-aether"             % "1.6.0",
      "org.ops4j.pax.swissbox" % "pax-swissbox-framework"     % "1.5.1",
      "org.apache.felix"       % "org.apache.felix.framework" % "3.2.2"
    )
  }

  /* Custom Settings */
  val repoKind = SettingKey[String]("repo-kind", "Maven repository kind (\"snapshots\" or \"releases\")")

  val publishedScalaSettings = Seq(
    scalaVersion := scalaVersions.head,
    crossScalaVersions := scalaVersions,
    //scalaBinaryVersion <<= scalaVersion,
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "optional")
  )

  def localScalaSettings(path: String): Seq[Setting[_]] = Seq(
    scalaVersion := "2.10.0-unknown",
    scalaBinaryVersion := "2.10.0-unknown",
    crossVersion := CrossVersion.Disabled,
    scalaHome := Some(file(path)),
    autoScalaLibrary := false,
    unmanagedJars <<= scalaInstance.map( _.jars.classpath),
    unmanagedJars in config("compile") <<= scalaInstance.map( _.jars.classpath),
    unmanagedJars in config("test") <<= scalaInstance.map( _.jars.classpath),
    unmanagedJars in config("macro") <<= scalaInstance.map( _.jars.classpath)
  )

  val scalaSettings = {
    sys.props("scala.home.local") match {
      case null => publishedScalaSettings
      case path =>
        scala.Console.err.println("Using local scala at " + path)
        localScalaSettings(path)
    }
  }

  def ifPublished(s: Seq[Setting[_]]): Seq[Setting[_]] =
    if(scalaSettings eq publishedScalaSettings) s else Nil

  def extTarget(extName: String, t: Option[String]): Seq[Setting[File]] = {
    sys.props("slick.build.target") match {
      case null => t.map(f => Seq(target := file(f))).getOrElse(Seq.empty)
      case path => Seq(target := file(path + "/" + extName))
    }
  }

  lazy val sharedSettings = Seq(
    version := slickVersion,
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= List("-deprecation", "-feature"),
    scalacOptions in (Compile, doc) <++= (version,sourceDirectory in Compile,name).map((v,src,n) => Seq(
      "-doc-title", n,
      "-doc-version", v,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", src.getPath, // needed for scaladoc to strip the location of the linked source path
      "-doc-source-url", "https://github.com/slick/slick/blob/"+v+"/src/main€{FILE_PATH}.scala",
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    )),
    libraryDependencies ++= Dependencies.mainDependencies,
    logBuffered := false,
    repoKind <<= (version)(v => if(v.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"),
    //publishTo <<= (repoKind)(r => Some(Resolver.file("test", file("c:/temp/repo/"+r)))),
    publishTo <<= (repoKind){
      case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
      case "releases" =>  Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime, Optional))) },
    homepage := Some(url("http://slick.typesafe.com")),
    startYear := Some(2008),
    licenses += ("Two-clause BSD-style license", url("http://github.com/slick/slick/blob/master/LICENSE.txt")),
    pomExtra :=
      <developers>
        <developer>
          <id>szeiger</id>
          <name>Stefan Zeiger</name>
          <timezone>+1</timezone>
          <url>http://szeiger.de</url>
        </developer>
        <developer>
          <id>cvogt</id>
          <name>Jan Christopher Vogt</name>
          <timezone>+1</timezone>
          <url>https://github.com/cvogt/</url>
        </developer>
      </developers>
        <scm>
          <url>git@github.com:slick/slick.git</url>
          <connection>scm:git:git@github.com:slick/slick.git</connection>
        </scm>
  ) ++ scalaSettings

  def runTasksSequentially(tasks: List[TaskKey[_]])(state: State): State = tasks match {
    case t :: ts =>
      Project.runTask(t.asInstanceOf[TaskKey[Any]], state) match {
        case None => state.fail
        case Some((s, Inc(_))) => s.fail
        case Some((s, _)) => runTasksSequentially(ts)(s)
      }
    case Nil => state
  }

  /* A command that runs 'testkit/test:test' and 'testkit/doctest:test' sequentially */
  def testAll = Command.command("testAll")(runTasksSequentially(List(
    test in (slickTestkitProject, Test),
    test in (slickTestkitProject, DocTest),
    test in (osgiTestProject, Test),
    sdlc in slickProject,
    sdlc in slickCodegenProject
  )))

  /* Project Definitions */
  lazy val aRootProject = Project(id = "root", base = file("."),
    settings = Project.defaultSettings ++ sharedSettings ++ extTarget("root", Some("target/root")) ++ Seq(
      sourceDirectory := file("target/root-src"),
      publishArtifact := false,
      test := (), // suppress test status output
      testOnly :=  (),
      commands += testAll
    )).aggregate(slickProject, slickCodegenProject, slickTestkitProject)

  lazy val slickProject: Project = Project(id = "slick", base = file("."),
    settings = Project.defaultSettings ++ sdlcSettings ++ inConfig(config("macro"))(Defaults.configSettings) ++ sharedSettings ++ fmppSettings ++ site.settings ++ site.sphinxSupport() ++ mimaDefaultSettings ++ extTarget("slick", None) ++ osgiSettings ++ Seq(
      name := "Slick",
      description := "Scala Language-Integrated Connection Kit",
      scalacOptions in (Compile, doc) <++= version.map(v => Seq(
        "-doc-source-url", "https://github.com/slick/slick/blob/"+v+"/src/main€{FILE_PATH}.scala",
        "-doc-root-content", "scaladoc-root.txt"
      )),
      (sphinxEnv in Sphinx) := (sphinxEnv in Sphinx).value +
        ("version" -> version.value.replaceFirst("""(\d*.\d*).*""", """$1""")) +
        ("release" -> version.value),
      (sphinxProperties in Sphinx) := Map.empty,
      makeSite <<= makeSite dependsOn (buildCapabilitiesTable in slickTestkitProject),
      sdlcBase := s"http://slick.typesafe.com/doc/${version.value}/api/",
      sdlcCheckDir := (target in com.typesafe.sbt.SbtSite.SiteKeys.makeSite).value,
      sdlc <<= sdlc dependsOn (doc in Compile, com.typesafe.sbt.SbtSite.SiteKeys.makeSite),
      test := (), // suppress test status output
      testOnly :=  (),
      previousArtifact := Some("com.typesafe.slick" %% "slick" % binaryCompatSlickVersion),
      binaryIssueFilters ++= Seq(
        ProblemFilters.exclude[MissingClassProblem]("scala.slick.util.MacroSupportInterpolationImpl$"),
        ProblemFilters.exclude[MissingClassProblem]("scala.slick.util.MacroSupportInterpolationImpl")
      ),
      ivyConfigurations += config("macro").hide.extend(Compile),
      unmanagedClasspath in Compile <++= fullClasspath in config("macro"),
      mappings in (Compile, packageSrc) <++= mappings in (config("macro"), packageSrc),
      mappings in (Compile, packageBin) <++= mappings in (config("macro"), packageBin),
      OsgiKeys.exportPackage := Seq("scala.slick", "scala.slick.*"),
      OsgiKeys.importPackage := Seq(
        osgiImport("scala*", scalaVersion.value),
        "*"
      ),
      OsgiKeys.privatePackage := Nil
    ) ++ ifPublished(Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "macro")
    )))

  /** Create an OSGi version range for standard Scala / Typesafe versioning
    * schemes that describes binary compatible versions. */
  def osgiVersionRange(version: String, requireMicro: Boolean = false): String =
    if(version contains '-') "${@}" // M, RC or SNAPSHOT -> exact version
    else if(requireMicro) "${range;[===,=+)}" // At least the same micro version
    else "${range;[==,=+)}" // Any binary compatible version

  /** Create an OSGi Import-Package version specification. */
  def osgiImport(pattern: String, version: String, requireMicro: Boolean = false): String =
    pattern + ";version=\"" + osgiVersionRange(version, requireMicro) + "\""

  val testKitTestCodegenDependencies = Dependencies.logback +: Dependencies.testDBs

  lazy val slickTestkitProject = Project(id = "testkit", base = file("slick-testkit"),
    settings = Project.defaultSettings ++ typeProvidersSettings ++ sharedSettings ++ extTarget("testkit", None) ++ Seq(
      name := "Slick-TestKit",
      description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
      scalacOptions in (Compile, doc) <++= version.map(v => Seq(
        "-doc-source-url", "https://github.com/slick/slick/blob/"+v+"/slick-testkit/src/main€{FILE_PATH}.scala"
      )),
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
      //scalacOptions in Compile += "-Yreify-copypaste",
      libraryDependencies ++=
        ("postgresql" % "postgresql" % "9.1-901.jdbc4" % "test") +:
        ("mysql" % "mysql-connector-java" % "5.1.23" % "test") +:
        Dependencies.junit ++:
        (Dependencies.logback +: Dependencies.testDBs).map(_ % "test") ++:
        (Dependencies.logback +: Dependencies.testDBs).map(_ % "codegen"),
      // Run the Queryable tests (which need macros) on a forked JVM
      // to avoid classloader problems with reification
      testGrouping <<= definedTests in Test map partitionTests,
      // Workaround for sbt bug: Without a testGrouping for all test configs,
      // the wrong tests are run
      testGrouping in DocTest <<= definedTests in DocTest map partitionTests,
      parallelExecution in Test := false,
      fork in run := true,
      javaOptions in run += "-Dslick.ansiDump=true",
      //javaOptions in run += "-verbose:gc",
      compile in Test ~= { a =>
        // Delete classes in "compile" packages after compiling. (Currently only scala.slick.test.compile.NestedShapeTest)
        // These are used for compile-time tests and should be recompiled every time.
        val products = a.relations.allProducts.toSeq ** new SimpleFileFilter(_.getParentFile.getName == "compile")
        IO.delete(products.get)
        a
      },
      buildCapabilitiesTable := {
        val logger = ConsoleLogger()
        Run.executeTrapExit({
          Run.run( "com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
            (fullClasspath in Compile).value.map(_.data),
            Seq("src/sphinx/capabilities.csv"),
            logger)(runner.value)
        }, logger)
      }
    ) ++ ifPublished(Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ % "test")
    ))
  ).configs(DocTest).settings(inConfig(DocTest)(Defaults.testSettings): _*).settings(
    unmanagedSourceDirectories in DocTest <+= (baseDirectory in slickProject) { _ / "src/sphinx/code" }
    //resourceDirectory in DocTest <<= baseDirectory { _ / "src/test/resources" }
    //test <<= Seq(test in Test, test in DocTest).dependOn,
    //concurrentRestrictions += Tags.limitSum(1, Tags.Test, Tags.ForkedTestGroup),
    //concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
  ) dependsOn(slickProject, slickCodegenProject)

  lazy val slickCodegenProject = Project(id = "codegen", base = file("slick-codegen"),
    settings = Project.defaultSettings ++ sdlcSettings ++ sharedSettings ++ extTarget("codegen", None) ++ Seq(
      name := "Slick-CodeGen",
      description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
      scalacOptions in (Compile, doc) <++= version.map(v => Seq(
        "-doc-source-url", "https://github.com/slick/slick/blob/"+v+"/slick-codegen/src/main€{FILE_PATH}.scala"
      )),
      sdlcBase := s"http://slick.typesafe.com/doc/${version.value}/codegen-api/",
      sdlcCheckDir := (target in (slickProject, com.typesafe.sbt.SbtSite.SiteKeys.makeSite)).value,
      sdlc <<= sdlc dependsOn (doc in Compile, com.typesafe.sbt.SbtSite.SiteKeys.makeSite in slickProject)
    )
  ) dependsOn(slickProject)

  lazy val osgiBundleFiles = taskKey[Seq[File]]("osgi-bundles that our tests rely on using.")

  lazy val osgiTestProject = (
    Project(id = "osgitests", base = file("osgi-tests"))
    settings(sharedSettings:_*)
    settings(
      name := "Slick-OsgiTests",
      libraryDependencies ++= (Dependencies.h2 +: Dependencies.logback +: Dependencies.junit ++: Dependencies.paxExam).map(_ % "test"),
      fork in Test := true,
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
      javaOptions in Test ++= Seq(
        "-Dslick.osgi.bundlepath=" + osgiBundleFiles.value.map(_.getCanonicalPath).mkString(":"),
        "-Dorg.ops4j.pax.logging.DefaultServiceLog.level=WARN"
      ),
      testGrouping <<= definedTests in Test map partitionTests,
      osgiBundleFiles := Seq((OsgiKeys.bundle in slickProject).value),
      osgiBundleFiles ++= (dependencyClasspath in Compile in slickProject).value.map(_.data).filterNot(_.isDirectory),
      osgiBundleFiles ++= (dependencyClasspath in Test).value.map(_.data).filter(f => f.name.contains("logback-") || f.name.contains("h2")),
      publishArtifact := false
    )
    dependsOn(slickProject % "test")
  )

  /* Test Configuration for running tests on doc sources */
  lazy val DocTest = config("doctest") extend(Test)

  /* Split tests into a group that needs to be forked and another one that can run in-process */
  def partitionTests(tests: Seq[TestDefinition]) = {
    val (fork, notFork) = tests partition (_.name contains ".queryable.")
    Seq(
      new Group("fork", fork, SubProcess(ForkOptions())),
      new Group("inProcess", notFork, InProcess)
    )
  }

  lazy val buildCapabilitiesTable = taskKey[Unit]("Build the capabilities.csv table for the documentation")

  /* FMPP Task */
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppConfig = config("fmpp") hide
  lazy val fmppSettings = inConfig(Compile)(Seq(sourceGenerators <+= fmpp, fmpp <<= fmppTask)) ++ Seq(
    libraryDependencies ++= Seq(
      ("net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name).intransitive,
      "org.freemarker" % "freemarker" % "2.3.20" % fmppConfig.name,
      "oro" % "oro" % "2.0.8" % fmppConfig.name,
      "org.beanshell" % "bsh" % "2.0b5" % fmppConfig.name,
      "xml-resolver" % "xml-resolver" % "1.2" % fmppConfig.name
    ),
    ivyConfigurations += fmppConfig,
    fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank },
    //mappings in (Compile, packageSrc) <++= // Add generated sources to sources JAR
    //  (sourceManaged in Compile, managedSources in Compile) map { (b, s) => s x (Path.relativeTo(b) | Path.flat) }
    mappings in (Compile, packageSrc) <++=
      (sourceManaged in Compile, managedSources in Compile, sourceDirectory in Compile) map { (base, srcs, srcDir) =>
        val fmppSrc = srcDir / "scala"
        val inFiles = fmppSrc ** "*.fm"
        (srcs x (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles x (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
      }
  )
  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, sourceManaged, streams, sourceDirectory) map { (cp, r, output, s, srcDir) =>
      val fmppSrc = srcDir / "scala"
      val inFiles = (fmppSrc ** "*.fm" get).toSet
      val cachedFun = FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete(output ** "*.scala" get)
        val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
          "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
        toError(r.run("fmpp.tools.CommandLine", cp.files, args, s.log))
        (output ** "*.scala").get.toSet
      }
      cachedFun(inFiles).toSeq
    }

  /** Slick type provider code gen  */
  lazy val typeProviders = taskKey[Seq[File]]("Type provider code generation")
  lazy val typeProvidersConfig = config("codegen").hide
  lazy val typeProvidersSettings = {
    inConfig(typeProvidersConfig)(Defaults.configSettings) ++
    Seq(
      sourceGenerators in Test <+= typeProviders,
      typeProviders <<= typeProvidersTask,
      ivyConfigurations += typeProvidersConfig.extend(Compile),      

      (compile in Test) <<= (compile in Test) dependsOn (compile in typeProvidersConfig),

      unmanagedClasspath in typeProvidersConfig <++= fullClasspath in (slickCodegenProject, Test),
      unmanagedClasspath in Test <++= fullClasspath in typeProvidersConfig,
      //mappings in (Test, packageSrc) <++= mappings in (typeProvidersConfig, packageSrc),
      //mappings in (Test, packageBin) <++= mappings in (typeProvidersConfig, packageBin),

      mappings in (Test, packageSrc) <++=
        (sourceManaged in Test, managedSources in Test, sourceDirectory in Test) map { (base, srcs, srcDir) =>
          val src = srcDir / "codegen"
          val inFiles = src ** "*.scala"
          (srcs x (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
            (inFiles x (Path.relativeTo(src) | Path.flat)) // Add *.fm files to sources JAR
        }
    )
  }
  lazy val typeProvidersTask =
    (fullClasspath in typeProvidersConfig, runner in typeProviders, sourceManaged in Test, streams, sourceDirectory, sourceDirectory in slickProject) map { (cp, r, output, s, srcDir, slickSrc) =>
      val src = srcDir / "codegen"
      val outDir = (output/"slick-codegen").getPath 
      val inFiles = (src ** "*.scala" get).toSet ++ (slickSrc / "main/scala/scala/slick/codegen" ** "*.scala" get).toSet ++ (slickSrc / "main/scala/scala/slick/jdbc/meta" ** "*.scala" get).toSet
      val cachedFun = FileFunction.cached(s.cacheDirectory / "type-providers", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete(output ** "*.scala" get)
        toError(r.run("scala.slick.test.codegen.CodeGeneratorTest", cp.files, Array(outDir, "scala.slick.test.codegen.generated"), s.log))
        (output ** "*.scala").get.toSet
      }
      cachedFun(inFiles).toSeq
    }
}

import sbt._
import Keys._
import Tests._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaBinaryIssueFilters, mimaReportBinaryIssues}
import com.typesafe.tools.mima.core.{ProblemFilters, MissingClassProblem}
import com.typesafe.sbt.osgi.SbtOsgi.{osgiSettings, OsgiKeys}
import com.typesafe.sbt.sdlc.Plugin._
import com.typesafe.sbt.pgp.PgpKeys
import com.novocode.ornate.sbtplugin.OrnatePlugin
import com.novocode.ornate.sbtplugin.OrnatePlugin.autoImport._

object SlickBuild extends Build {

  // NOTE: remember to change the version numbers in the sample projects
  // when changing them here

  val slickVersion = "3.3.0-SNAPSHOT"
  val binaryCompatSlickVersion = "3.3.0" // Slick base version for binary compatibility checks
  val scalaVersions = Seq("2.11.12", "2.12.4")

  /** Dependencies for reuse in different parts of the build */
  object Dependencies {
    val junit = Seq(
      "junit" % "junit-dep" % "4.11",
      "com.novocode" % "junit-interface" % "0.11"
    )
    def scalaTestFor(scalaVersion: String) = {
      val v = "3.0.4"
      "org.scalatest" %% "scalatest" % v
    }
    val slf4j = "org.slf4j" % "slf4j-api" % "1.7.25"
    val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
    val typesafeConfig = "com.typesafe" % "config" % "1.3.2"
    val reactiveStreamsVersion = "1.0.1"
    val reactiveStreams = "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
    val reactiveStreamsTCK = "org.reactivestreams" % "reactive-streams-tck" % reactiveStreamsVersion
    val hikariCP = "com.zaxxer" % "HikariCP" % "2.7.4"
    val mainDependencies = Seq(slf4j, typesafeConfig, reactiveStreams)
    val h2 = "com.h2database" % "h2" % "1.4.197"
    val testDBs = Seq(
      h2,
      "org.apache.derby" % "derby" % "10.11.1.1",
      "org.xerial" % "sqlite-jdbc" % "3.8.11.2",
      "org.hsqldb" % "hsqldb" % "2.2.8",
      "org.postgresql" % "postgresql" % "42.2.2",
      "mysql" % "mysql-connector-java" % "5.1.38",
      "net.sourceforge.jtds" % "jtds" % "1.3.1"
    )
    val paxExamVersion = "4.6.0"
    val paxExam = Seq(
      "org.ops4j.pax.exam"     % "pax-exam-container-native"  % paxExamVersion,
      "org.ops4j.pax.exam"     % "pax-exam-junit4"            % paxExamVersion,
      "org.ops4j.pax.exam"     % "pax-exam-link-assembly"     % paxExamVersion,
      "org.ops4j.pax.url"      % "pax-url-aether"             % "1.6.0",
      "org.ops4j.pax.swissbox" % "pax-swissbox-framework"     % "1.5.1",
      "org.apache.felix"       % "org.apache.felix.framework" % "4.6.1"
    )
  }

  /* Custom Settings */
  val repoKind = SettingKey[String]("repo-kind", "Maven repository kind (\"snapshots\" or \"releases\")")

  val testSamples = TaskKey[Unit]("testSamples", "Run tests in the sample apps")

  val publishedScalaSettings = Seq(
    scalaVersion := scalaVersions.head,
    crossScalaVersions := scalaVersions
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

  def extTarget(extName: String): Seq[Setting[File]] = {
    sys.props("slick.build.target") match {
      case null => Seq.empty
      case path => Seq(target := file(path + "/" + extName))
    }
  }

  val makeSite = TaskKey[Unit]("makeSite")

  def versionTag(v: String) = // get the tag for a version
    "v" + v

  lazy val sharedSettings = Seq(
    version := slickVersion,
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= List("-deprecation", "-feature", "-unchecked", "-Xfuture"),
    scalacOptions in (Compile, doc) ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (sourceDirectory in Compile).value.getPath, // needed for scaladoc to strip the location of the linked source path
      "-doc-source-url", s"https://github.com/slick/slick/blob/${versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    ),
    logBuffered := false,
    repoKind := (if (version.value.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"),
    publishTo := (repoKind.value match {
      case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
      case "releases" =>  Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    }),
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
  ) ++ scalaSettings

  def commonSdlcSettings = Seq(
    sdlcBase := (projectID.value.name + "-api/").replaceFirst("^slick-", ""),
    sdlcCheckDir := (ornateTargetDir in aRootProject).value.get,
    sdlc := (sdlc dependsOn (doc in Compile, makeSite in aRootProject)).value
  )

  def runTasksSequentially(tasks: List[TaskKey[_]])(state: State): State = tasks match {
    case t :: ts =>
      Project.runTask(t.asInstanceOf[TaskKey[Any]], state) match {
        case None => state.fail
        case Some((s, Inc(_))) => s.fail
        case Some((s, _)) => runTasksSequentially(ts)(s)
      }
    case Nil => state
  }

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
      // mimaReportBinaryIssues in Compile in slickProject, // enable for minor versions
      testSamples in aRootProject
    )
    val withSdlc =
      /*if(extracted.get(scalaVersion).startsWith("2.11.")) tasks :+ (sdlc in aRootProject)
      else*/ tasks
    runTasksSequentially(withSdlc)(state)
  }

  /* Project Definitions */
  lazy val aRootProject: Project = Project(id = "root", base = file("."),
    settings = Defaults.coreDefaultSettings ++ sharedSettings ++ extTarget("root") ++ Seq(
      sourceDirectory := file("target/root-src"),
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      PgpKeys.publishSigned := {},
      PgpKeys.publishLocalSigned := {},
      test := (), testOnly :=  (), // suppress test status output
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
      },
      ornateBaseDir := Some(file("doc")),
      ornateSourceDir := Some(file("doc/src")),
      ornateTargetDir := Some(file("doc/target")),
      cleanFiles += file("doc/target"),
      ornateResourceDir := Some(file("doc/resources")),
      ornateSettings := Map(
        "version" -> version.value,
        "shortVersion" -> version.value.replaceFirst("""(\d*.\d*).*""", """$1"""),
        "tag" -> versionTag(version.value), // for snippet links
        "branch" -> "3.2", // for "Edit page" links
        "scalaVersion" -> scalaVersion.value // for "scalaapi:" links
      ),
      ornate := (ornate dependsOn (buildCapabilitiesTable in slickTestkitProject)).value,
      sdlc := (),
      sdlc := (sdlc dependsOn (sdlc in slickProject, sdlc in slickCodegenProject, sdlc in slickHikariCPProject)).value,
      makeSite := {
        val __ = ornate.value
        def cp(s: File, t: File) = {
          IO.delete(t)
          IO.createDirectory(t)
          IO.copyDirectory(s, t)
        }
        cp((doc in Compile in slickProject).value, file("doc/target/api"))
        cp((doc in Compile in slickCodegenProject).value, file("doc/target/codegen-api"))
        cp((doc in Compile in slickHikariCPProject).value, file("doc/target/hikaricp-api"))
        cp((doc in Compile in slickTestkitProject).value, file("doc/target/testkit-api"))
      }
    ))
    .enablePlugins(OrnatePlugin)
    .aggregate(slickProject, slickCodegenProject, slickHikariCPProject, slickTestkitProject)

  lazy val slickProject: Project = Project(id = "slick", base = file("slick"),
    settings = Defaults.coreDefaultSettings ++ sdlcSettings ++ inConfig(config("macro"))(Defaults.configSettings) ++ sharedSettings ++ fmppSettings ++ mimaDefaultSettings ++ extTarget("slick") ++ commonSdlcSettings ++ osgiSettings ++ Seq(
      name := "Slick",
      description := "Scala Language-Integrated Connection Kit",
      libraryDependencies ++= Dependencies.mainDependencies,
      scalacOptions in (Compile, doc) ++= Seq(
        "-doc-source-url", s"https://github.com/slick/slick/blob/${versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
        "-doc-root-content", "scaladoc-root.txt"
      ),
      test := (), testOnly :=  (), // suppress test status output
      mimaPreviousArtifacts := Set("com.typesafe.slick" % ("slick_" + scalaBinaryVersion.value) % binaryCompatSlickVersion),
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
      OsgiKeys.importPackage := Seq(
        osgiImport("scala*", scalaVersion.value),
        "*"
      ),
      OsgiKeys.privatePackage := Nil
    ) ++ ifPublished(Seq(
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "macro"
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
    settings = Defaults.coreDefaultSettings ++ typeProvidersSettings ++ sharedSettings ++ extTarget("testkit") ++ Seq(
      name := "Slick-TestKit",
      description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
      scalacOptions in (Compile, doc) ++= Seq(
        "-doc-source-url", s"https://github.com/slick/slick/blob/${versionTag(version.value)}/slick-testkit/src/main€{FILE_PATH}.scala"
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
      buildCapabilitiesTable := {
        val logger = ConsoleLogger()
        Run.run( "com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
          (fullClasspath in Compile).value.map(_.data),
          Seq("doc/capabilities.md"),
          logger)(runner.value)
      }
    ) ++ ifPublished(Seq(
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
    ))
  ).configs(DocTest).settings(inConfig(DocTest)(Defaults.testSettings): _*).settings(
    unmanagedSourceDirectories in DocTest += (baseDirectory in LocalProject("root")).value / "doc/code",
    unmanagedResourceDirectories in DocTest += (baseDirectory in LocalProject("root")).value / "doc/code"
  ) dependsOn(slickProject, slickCodegenProject % "compile->compile", slickHikariCPProject)

  lazy val slickCodegenProject = Project(id = "codegen", base = file("slick-codegen"),
    settings = Defaults.coreDefaultSettings ++ sdlcSettings ++ sharedSettings ++ extTarget("codegen") ++ commonSdlcSettings ++ Seq(
      name := "Slick-CodeGen",
      description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
      scalacOptions in (Compile, doc) ++= Seq(
        "-doc-source-url", s"https://github.com/slick/slick/blob/${versionTag(version.value)}/slick-codegen/src/main€{FILE_PATH}.scala"
      ),
      unmanagedResourceDirectories in Test += (baseDirectory in aRootProject).value / "common-test-resources",
      test := (), testOnly :=  () // suppress test status output
    )
  ) dependsOn(slickProject)

  lazy val slickHikariCPProject = Project(id = "hikaricp", base = file("slick-hikaricp"),
    settings = Defaults.coreDefaultSettings ++ sdlcSettings ++ sharedSettings ++ extTarget("hikaricp") ++ commonSdlcSettings ++ osgiSettings ++ Seq(
      name := "Slick-HikariCP",
      description := "HikariCP integration for Slick (Scala Language-Integrated Connection Kit)",
      scalacOptions in (Compile, doc) ++= Seq(
        "-doc-source-url", s"https://github.com/slick/slick/blob/${versionTag(version.value)}/slick-hikaricp/src/main€{FILE_PATH}.scala"
      ),
      libraryDependencies += Dependencies.hikariCP,
      test := (), testOnly := (), // suppress test status output
      OsgiKeys.exportPackage := Seq("slick.jdbc.hikaricp"),
      OsgiKeys.importPackage := Seq(
        osgiImport("slick*", slickVersion),
        osgiImport("scala*", scalaVersion.value),
        "*"
      ),
      OsgiKeys.privatePackage := Nil
    )
  ) dependsOn(slickProject)

  lazy val reactiveStreamsTestProject = Project(id = "reactive-streams-tests", base = file("reactive-streams-tests"),
    settings = Defaults.coreDefaultSettings ++ sharedSettings ++ Seq(
      name := "Slick-ReactiveStreamsTests",
      unmanagedResourceDirectories in Test += (baseDirectory in aRootProject).value / "common-test-resources",
      resolvers += Resolver.sbtPluginRepo("releases"),
      libraryDependencies += Dependencies.scalaTestFor(scalaVersion.value),
      libraryDependencies ++= (Dependencies.logback +: Dependencies.testDBs).map(_ % "test"),
      libraryDependencies += Dependencies.reactiveStreamsTCK,
      parallelExecution in Test := false
    )
  ) dependsOn(slickTestkitProject)

  def sampleProject(s: String): Project = Project(id = "sample-"+s, base = file("samples/"+s))
    .dependsOn(slickProject)
    .addSbtFiles(file("../override.sbt"))

  lazy val sampleHelloSlickProject = sampleProject("hello-slick")

  lazy val sampleSlickMultidbProject = sampleProject("slick-multidb")

  lazy val sampleSlickPlainsqlProject = sampleProject("slick-plainsql")

  lazy val sampleSlickTestkitExampleProject = sampleProject("slick-testkit-example").dependsOn(slickTestkitProject)

  lazy val osgiBundleFiles = taskKey[Seq[File]]("osgi-bundles that our tests rely on using.")

  lazy val osgiTestProject = (
    Project(id = "osgitests", base = file("osgi-tests"))
    settings(sharedSettings:_*)
    settings(
      name := "Slick-OsgiTests",
      libraryDependencies ++= (Dependencies.h2 +: Dependencies.logback +: Dependencies.junit ++: Dependencies.reactiveStreams +: Dependencies.paxExam).map(_ % "test"),
      unmanagedResourceDirectories in Test += (baseDirectory in aRootProject).value / "common-test-resources",
      fork in Test := true,
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
      javaOptions in Test ++= Seq(
        // Use '@' as a seperator that shouldn't appear in any filepaths or names
        "-Dslick.osgi.bundlepath=" + osgiBundleFiles.value.map(_.getCanonicalPath).mkString("@"),
        "-Dorg.ops4j.pax.logging.DefaultServiceLog.level=WARN"
      ),
      osgiBundleFiles := Seq((OsgiKeys.bundle in slickProject).value),
      osgiBundleFiles ++= (dependencyClasspath in Compile in slickProject).value.map(_.data).filterNot(_.isDirectory),
      osgiBundleFiles ++= (dependencyClasspath in Test).value.map(_.data).filter(f => f.name.contains("logback-") || f.name.contains("h2")),
      publishArtifact := false
    )
    dependsOn(slickProject % "test")
  )

  /* Test Configuration for running tests on doc sources */
  lazy val DocTest = config("doctest") extend(Test)

  lazy val buildCapabilitiesTable = taskKey[Unit]("Build the capabilities.csv table for the documentation")

  /* FMPP Task */
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppConfig = config("fmpp").hide
  lazy val fmppSettings = inConfig(Compile)(Seq(sourceGenerators += fmpp.taskValue, fmpp := fmppTask.value)) ++ Seq(
    libraryDependencies ++= Seq(
      ("net.sourceforge.fmpp" % "fmpp" % "0.9.15" % fmppConfig.name).intransitive,
      "org.freemarker" % "freemarker" % "2.3.23" % fmppConfig.name,
      "oro" % "oro" % "2.0.8" % fmppConfig.name,
      "org.beanshell" % "bsh" % "2.0b5" % fmppConfig.name,
      "xml-resolver" % "xml-resolver" % "1.2" % fmppConfig.name
    ),
    ivyConfigurations += fmppConfig,
    fullClasspath in fmppConfig := update.map { _ select configurationFilter(fmppConfig.name) map Attributed.blank }.value,
    mappings in (Compile, packageSrc) ++= {
      val fmppSrc = (sourceDirectory in Compile).value / "scala"
      val inFiles = fmppSrc ** "*.fm"
      ((managedSources in Compile).value.pair(Path.relativeTo((sourceManaged in Compile).value) | Path.flat)) ++ // Add generated sources to sources JAR
        (inFiles pair (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
    }
  )
  lazy val fmppTask = Def.task {
    val s = streams.value
    val output = sourceManaged.value
    val fmppSrc = sourceDirectory.value / "scala"
    val inFiles = (fmppSrc ** "*.fm").get.toSet
    val cachedFun = FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
      IO.delete((output ** "*.scala").get)
      val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
        "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
      toError((runner in fmpp).value.run("fmpp.tools.CommandLine", (fullClasspath in fmppConfig).value.files, args, s.log))
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
      sourceGenerators in Test += typeProviders.taskValue,
      typeProviders := typeProvidersTask.value,
      ivyConfigurations += typeProvidersConfig.extend(Compile),
      (compile in Test) := ((compile in Test) dependsOn (compile in typeProvidersConfig)).value,
      unmanagedClasspath in typeProvidersConfig ++= (fullClasspath in config("compile")).value,
      unmanagedClasspath in typeProvidersConfig ++= (fullClasspath in (slickCodegenProject, Test)).value,
      unmanagedClasspath in Test ++= (fullClasspath in typeProvidersConfig).value,
      mappings in (Test, packageSrc) ++= {
        val src = (sourceDirectory in Test).value / "codegen"
        val inFiles = src ** "*.scala"
        ((managedSources in Test).value.pair(Path.relativeTo((sourceManaged in Test).value) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles pair (Path.relativeTo(src) | Path.flat)) // Add *.fm files to sources JAR
      }
    )
  }
  lazy val typeProvidersTask = Def.task {
    val cp = (fullClasspath in typeProvidersConfig).value
    val r = (runner in typeProviders).value
    val output = (sourceManaged in Test).value
    val s = streams.value
    val srcDir = sourceDirectory.value
    val slickSrc = (sourceDirectory in slickProject).value
    val src = srcDir / "codegen"
    val outDir = (output/"slick-codegen").getPath
    val inFiles = (src ** "*.scala").get.toSet ++ (slickSrc / "main/scala/slick/codegen" ** "*.scala").get.toSet ++ (slickSrc / "main/scala/slick/jdbc/meta" ** "*.scala").get.toSet
    val cachedFun = FileFunction.cached(s.cacheDirectory / "type-providers", outStyle = FilesInfo.exists) { (in: Set[File]) =>
      IO.delete((output ** "*.scala").get)
      toError(r.run("slick.test.codegen.GenerateMainSources", cp.files, Array(outDir), s.log))
      toError(r.run("slick.test.codegen.GenerateRoundtripSources", cp.files, Array(outDir), s.log))
      (output ** "*.scala").get.toSet
    }
    cachedFun(inFiles).toSeq
  }
}

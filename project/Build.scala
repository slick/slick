import sbt._
import Keys._
import Tests._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.sbt.osgi.SbtOsgi.osgiSettings
import com.typesafe.sbt.sdlc.Plugin._
import de.johoop.testngplugin.TestNGPlugin._

object SlickBuild extends Build {

  val slickVersion = "3.0.0-SNAPSHOT"
  val binaryCompatSlickVersion = "3.0.0" // Slick base version for binary compatibility checks
  val scalaVersions = Seq("2.10.4", "2.11.4")

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
  def testAll = Command.command("testAll")(runTasksSequentially(List(
    test in (slickTestkit, Test),
    test in (slickDocs, Test),
    test in (osgiTest, Test), // Temporarily disabled until we get Reactive Streams OSGi bundles
    test in (reactiveStreamsTest, Test)
    //sdlc in aRootProject
  )))

  /* Project Definitions */
  val slick = Project(
    id = "slick",
    base = file("slick"),
    settings = /*sdlcSettings ++*/ inConfig(config("macro"))(Defaults.configSettings) ++ fmppSettings ++ mimaDefaultSettings ++ osgiSettings
  )

  val slickDirect = Project(
    id = "slick-direct",
    base = file("slick-direct"),
    //settings = /*sdlcSettings ++*/,
    dependencies = Seq(slick)
  )

  val slickCodegen = Project(
    id = "slick-codegen",
    base = file("slick-codegen"),
    //    settings = /*sdlcSettings ++*/,
    dependencies = Seq(slick)
  )

  val slickTestkit = Project(
    id = "slick-testkit",
    base = file("slick-testkit"),
    settings = typeProvidersSettings,
    dependencies = Seq(slick, slickCodegen % "compile->compile", slickDirect % "test->compile")
  )

  val reactiveStreamsTest = Project(
    id = "reactive-streams-tests",
    base = file("reactive-streams-tests"),
    settings = testNGSettings,
    dependencies = Seq(slickTestkit)
  )

  val slickDocs = Project(
    id = "slick-docs",
    base = file("slick-docs"),
    dependencies = Seq(slick, slickCodegen, slickDirect, slickTestkit % "test->test")
  )

  val osgiTest = Project(
    id = "osgitests",
    base = file("osgi-tests"),
    dependencies = Seq(slick % "test")
  )

  val root = Project(
    id = "root",
    base = file("."),
    settings = Seq(
      publishArtifact := false,
      test := (), testOnly :=  (), // suppress test status output
      commands += testAll
      //      sdlc := (),
      //      sdlc <<= sdlc dependsOn (sdlc in slickProject, sdlc in slickCodegenProject, sdlc in slickBlockingProject, sdlc in slickDirectProject)
    ),
    aggregate = Seq(slick, slickDocs, slickCodegen, slickDirect, slickTestkit)
  )


  lazy val osgiBundleFiles = taskKey[Seq[File]]("osgi-bundles that our tests rely on using.")
  /** Create an OSGi version range for standard Scala / Typesafe versioning
    * schemes that describes binary compatible versions. */
  def osgiVersionRange(version: String, requireMicro: Boolean = false): String =
    if(version contains '-') "${@}" // M, RC or SNAPSHOT -> exact version
    else if(requireMicro) "${range;[===,=+)}" // At least the same micro version
    else "${range;[==,=+)}" // Any binary compatible version

  /** Create an OSGi Import-Package version specification. */
  def osgiImport(pattern: String, version: String, requireMicro: Boolean = false): String =
    pattern + ";version=\"" + osgiVersionRange(version, requireMicro) + "\""

  /* Test Configuration for running tests on doc sources */
  lazy val DocTest = config("doctest").extend(Test)

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
  lazy val fmppConfig = config("fmpp").hide
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
        (srcs pair (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles pair (Path.relativeTo(fmppSrc) | Path.flat)) // Add *.fm files to sources JAR
      }
  )
  lazy val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, sourceManaged, streams, sourceDirectory) map { (cp, r, output, s, srcDir) =>
      val fmppSrc = srcDir / "scala"
      val inFiles = (fmppSrc ** "*.fm").get.toSet
      val cachedFun = FileFunction.cached(s.cacheDirectory / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete((output ** "*.scala").get)
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

      unmanagedClasspath in typeProvidersConfig <++= fullClasspath in config("compile"),
      unmanagedClasspath in typeProvidersConfig <++= fullClasspath in (slickCodegen, Test),
      unmanagedClasspath in Test <++= fullClasspath in typeProvidersConfig,
      //mappings in (Test, packageSrc) <++= mappings in (typeProvidersConfig, packageSrc),
      //mappings in (Test, packageBin) <++= mappings in (typeProvidersConfig, packageBin),

      mappings in (Test, packageSrc) <++=
        (sourceManaged in Test, managedSources in Test, sourceDirectory in Test) map { (base, srcs, srcDir) =>
          val src = srcDir / "codegen"
          val inFiles = src ** "*.scala"
          (srcs pair (Path.relativeTo(base) | Path.flat)) ++ // Add generated sources to sources JAR
            (inFiles pair (Path.relativeTo(src) | Path.flat)) // Add *.fm files to sources JAR
        }
    )
  }
  lazy val typeProvidersTask =
    (fullClasspath in typeProvidersConfig, runner in typeProviders, sourceManaged in Test, streams, sourceDirectory, sourceDirectory in slick) map { (cp, r, output, s, srcDir, slickSrc) =>
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

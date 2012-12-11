import sbt._
import Keys._
import Tests._
import com.jsuereth.sbtsite.SphinxSupport.Sphinx
import com.jsuereth.sbtsite.SitePlugin.site

object SlickBuild extends Build {

  /* Custom Settings */
  val repoKind = SettingKey[String]("repo-kind", "Maven repository kind (\"snapshots\" or \"releases\")")

  val publishedScalaSettings = Seq(
    scalaVersion := "2.10.0-RC3",
    scalaBinaryVersion <<= scalaVersion,
    //crossScalaVersions ++= "2.10.0-M4" :: Nil,
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies in config("macro") <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)
  )

  def localScalaSettings(path: String): Seq[Setting[_]] = Seq(
    scalaVersion := "2.10.0-unknown",
    scalaBinaryVersion := "2.10.0-unknown",
    crossVersion := CrossVersion.Disabled,
    scalaHome := Some(file(path)),
    autoScalaLibrary := false,
    unmanagedJars <<= scalaInstance.map( _.jars.classpath),
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

  lazy val sharedSettings = Seq(
    version := "1.1.0-SNAPSHOT",
    organizationName := "Typesafe",
    organization := "com.typesafe",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= List("-deprecation", "-feature"),
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.6.4",
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
    makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime))) },
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
        </scm>,
    includeFilter in Sphinx := ("*.html" | "*.png" | "*.js" | "*.css" | "*.gif" | "*.txt"),
    // Work around scaladoc problem
    unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
  ) ++ scalaSettings

  /* Project Definitions */
  lazy val aRootProject = Project(id = "root", base = file("."),
    settings = Project.defaultSettings ++ sharedSettings ++ Seq(
      target := file("target/root"),
      sourceDirectory := file("target/root-src"),
      publishArtifact := false,
      test := (),
      testOnly <<= inputTask { argTask => (argTask) map { args => }}
    )).aggregate(slickProject, slickTestkitProject)
  lazy val slickProject = Project(id = "slick", base = file("."),
    settings = Project.defaultSettings ++ inConfig(config("macro"))(Defaults.configSettings) ++ sharedSettings ++ fmppSettings ++ site.settings ++ site.sphinxSupport() ++ Seq(
      name := "Slick",
      description := "Scala Language-Integrated Connection Kit",
      scalacOptions in doc <++= (version).map(v => Seq("-doc-title", "Slick", "-doc-version", v)),
      test := (),
      testOnly <<= inputTask { argTask => (argTask) map { args => }},
      ivyConfigurations += config("macro").hide.extend(Compile),
      unmanagedClasspath in Compile <++= fullClasspath in config("macro"),
      mappings in (Compile, packageSrc) <++= mappings in (config("macro"), packageSrc),
      mappings in (Compile, packageBin) <++= mappings in (config("macro"), packageBin)
    ))
  lazy val slickTestkitProject = Project(id = "testkit", base = file("slick-testkit"),
    settings = Project.defaultSettings ++ sharedSettings ++ Seq(
      name := "Slick-TestKit",
      description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
      scalacOptions in doc <++= (version).map(v => Seq("-doc-title", "Slick TestKit", "-doc-version", v)),
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
      //scalacOptions in Compile += "-Yreify-copypaste",
      libraryDependencies ++= Seq(
        // TestKit needs JUnit for its Runner
        "junit" % "junit-dep" % "4.10",
        // The Slick core tests need junit-interface, logback and the DB drivers
        "com.novocode" % "junit-interface" % "0.10-M1" % "test",
        "ch.qos.logback" % "logback-classic" % "0.9.28" % "test",
        "com.h2database" % "h2" % "1.3.170" % "test",
        "org.xerial" % "sqlite-jdbc" % "3.7.2" % "test",
        "org.apache.derby" % "derby" % "10.9.1.0" % "test",
        "org.hsqldb" % "hsqldb" % "2.2.8" % "test",
        "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test",
        "mysql" % "mysql-connector-java" % "5.1.13" % "test",
        "net.sourceforge.jtds" % "jtds" % "1.2.4" % "test"
      ),
      // Run the Queryable tests (which need macros) on a forked JVM
      // to avoid classloader problems with reification
      testGrouping <<= definedTests in Test map partitionTests,
      // Workaround for sbt bug: Without a testGrouping for all test configs,
      // the wrong tests are run
      testGrouping in DocTest <<= definedTests in DocTest map partitionTests,
      parallelExecution in Test := false
    )
  ).configs(DocTest).settings(inConfig(DocTest)(Defaults.testSettings): _*).settings(
    unmanagedSourceDirectories in DocTest <+= (baseDirectory in slickProject) { _ / "src/sphinx/code" }
    //resourceDirectory in DocTest <<= baseDirectory { _ / "src/test/resources" }
    //test <<= Seq(test in Test, test in DocTest).dependOn,
    //concurrentRestrictions += Tags.limitSum(1, Tags.Test, Tags.ForkedTestGroup),
    //concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
  ) dependsOn(slickProject)

  /* Test Configuration for running tests on doc sources */
  lazy val DocTest = config("doctest") extend(Test)

  /* Split tests into a group that needs to be forked and another one that can run in-process */
  def partitionTests(tests: Seq[TestDefinition]) = {
    val (fork, notFork) = tests partition (_.name contains ".queryable.")
    Seq(
      new Group("fork", fork, SubProcess(Seq())),
      new Group("inProcess", notFork, InProcess)
    )
  }

  /* FMPP Task */
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  lazy val fmppConfig = config("fmpp") hide
  lazy val fmppSettings = inConfig(Compile)(Seq(sourceGenerators <+= fmpp, fmpp <<= fmppTask)) ++ Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name,
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
    (fullClasspath in fmppConfig, runner in fmpp, sourceManaged, streams, cacheDirectory, sourceDirectory) map { (cp, r, output, s, cache, srcDir) =>
      val fmppSrc = srcDir / "scala"
      val inFiles = (fmppSrc ** "*.fm" get).toSet
      val cachedFun = FileFunction.cached(cache / "fmpp", outStyle = FilesInfo.exists) { (in: Set[File]) =>
        IO.delete(output ** "*.scala" get)
        val args = "--expert" :: "-q" :: "-S" :: fmppSrc.getPath :: "-O" :: output.getPath ::
          "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil
        toError(r.run("fmpp.tools.CommandLine", cp.files, args, s.log))
        (output ** "*.scala").get.toSet
      }
      cachedFun(inFiles).toSeq
    }
}

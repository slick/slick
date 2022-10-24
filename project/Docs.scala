import java.nio.file.{Files, Paths}

import com.github.sbt.git.ConsoleGitRunner
import com.lightbend.paradox.sbt.ParadoxPlugin
import com.lightbend.paradox.sbt.ParadoxPlugin.autoImport._
import coursier.version.{Version, VersionParse}
import sbt.Keys._
import sbt._


object Docs extends AutoPlugin {
  object autoImport {
    val preprocessDocs = taskKey[File]("Prepare the documentation directory for Paradox")
    val checkScaladocLinks = taskKey[Unit]("Prepare the documentation directory for Paradox")
    val scaladocDirs = taskKey[Seq[(String, File)]]("Scaladoc directories to include with documentation")
    val addDocsToDocRepo = taskKey[Boolean]("Pull doc repo and add generated documentation to it")
    val deployDocs = taskKey[Unit]("Deploy docs to GitHub Pages")
    val showParadoxProperties = taskKey[Unit]("Show a table of paradoxProperties")
  }

  import autoImport._


  override def requires = ParadoxPlugin

  def modifyFileLines(file: File)(f: String => String): Unit =
    IO.writeLines(file, IO.readLines(file).map(f))

  lazy val docRepoCheckoutDir = {
    val dir = Paths.get("/var/tmp/slick-docs").toFile
    dir.deleteOnExit()
    dir
  }

  private def addDocsToDocRepoImpl(src: File, info: Versioning.MaybeVersionInfo, log: Logger) = {
    val dir = docRepoCheckoutDir
    val repo = "git@github.com:slick/doc.git"
    log.info(s"Cloning $repo into $dir")
    if (Option(dir.listFiles()).forall(_.isEmpty))
      ConsoleGitRunner("clone", "--branch=gh-pages", "--depth=1", repo, ".")(dir, log)
    else {
      ConsoleGitRunner("reset", "--hard")(dir, log)
      ConsoleGitRunner("clean", "-fd")(dir, log)
      ConsoleGitRunner("pull")(dir, log)
    }

    val versionString = docsSubdirectory(info)
    val dest = dir / versionString
    val existed = dest.exists()
    IO.delete(dest)
    log.info("Copying docs")
    IO.copyDirectory(src, dest)

    val version = Version(versionString)
    val versionNumberParts = version.items.takeWhile(Version.isNumeric)

    val versions =
      IO.listFiles(dir)
        .filter(_.isDirectory)
        .flatMap(f => VersionParse.version(f.getName))
        .toSeq
        .filter {
          case `version` => true
          case v         =>
            val (numberParts, otherParts) = v.items.span(Version.isNumeric)
            def isValidTag = {
              val tag = otherParts.collect { case tag: Version.Tag => tag.value }
              numberParts == versionNumberParts &&
                (tag == Seq("m") || tag == Seq("rc"))
            }
            otherParts.isEmpty || isValidTag

        }
        .sorted

    IO.write(
      dir / "versions.json",
      versions.map("  \"" + _.repr + '"').mkString("[\n", ",\n", "\n]\n")
    )

    info.maybeVersionInfo.map(_.versionType).foreach { versionType =>
      val label = versionType match {
        case Versioning.VersionInfo.VersionType.Devel                   => "devel"
        case Versioning.VersionInfo.VersionType.Tag.Prerelease(_, _, _) => "prerelease"
        case Versioning.VersionInfo.VersionType.Tag.Stable(_, _, _)     => "stable"
      }
      val path = (dir / label).toPath
      Files.deleteIfExists(path)
      Files.createSymbolicLink(path, dir.toPath.relativize(dest.toPath))
    }

    existed
  }

  private def docsSubdirectory(info: Versioning.MaybeVersionInfo) =
    info.maybeVersionInfo.fold("local")(_.shortVersionString)

  override def projectSettings = Seq(
    homepage := None,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    Compile / paradoxProperties ++= {
      val scaladocBaseUrl = s"https://scala-slick.org/doc/${docsSubdirectory(Versioning.maybeVersionInfo.value)}"
      val ref = Versioning.currentRef(baseDirectory.value)
      Map(
        "scaladoc.scala.base_url" -> s"https://www.scala-lang.org/api/${scalaVersion.value}",
        "scaladoc.slick.base_url" -> s"$scaladocBaseUrl/api",
        "scaladoc.slick.codegen.base_url" -> s"$scaladocBaseUrl/codegen-api",
        "scaladoc.slick.jdbc.hikaricp.base_url" -> s"$scaladocBaseUrl/hikaricp-api",
        "scaladoc.com.typesafe.slick.testkit.base_url" -> s"$scaladocBaseUrl/testkit-api",
        "javadoc.javax.sql.base_url" -> "https://docs.oracle.com/javase/8/docs/api/",
        "github.base_url" -> (scmInfo.value.get.browseUrl.toString + "/blob/main"),
        "extref.SI.base_url" -> "https://issues.scala-lang.org/browse/SI-%s",
        "extref.about-pool-sizing.base_url" -> "https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing",
        "extref.activator.base_url" -> "https://typesafe.com/activator",
        "extref.akka-sphinx.base_url" -> "https://doc.akka.io/docs/akka/2.4.0/dev/documentation.html",
        "extref.akka-streams.base_url" -> "https://akka.io/docs/",
        "extref.akka.base_url" -> "https://akka.io/",
        "extref.db2.base_url" -> "https://www.ibm.com/analytics/db2",
        "extref.derby.base_url" -> "https://db.apache.org/derby/",
        "extref.h2.base_url" -> "https://www.h2database.com/",
        "extref.hikaricp-monitoring.base_url" ->
          "https://github.com/brettwooldridge/HikariCP/wiki/MBean-(JMX)-Monitoring-and-Management",
        "extref.hikaricp.base_url" -> "https://github.com/brettwooldridge/HikariCP",
        "extref.hsqldb.base_url" -> "http://hsqldb.org/",
        "extref.javaapi.base_url" -> "https://docs.oracle.com/javase/7/docs/api/%s.html",
        "extref.javadb.base_url" -> "https://www.oracle.com/java/technologies/javadb.html",
        "extref.jdbc.base_url" -> "https://en.wikipedia.org/wiki/Java_Database_Connectivity",
        "extref.jmx.base_url" -> "https://en.wikipedia.org/wiki/Java_Management_Extensions",
        "extref.jpa.base_url" -> "https://en.wikipedia.org/wiki/Java_Persistence_API",
        "extref.lightbend.base_url" -> "https://www.lightbend.com/",
        "extref.logback.base_url" -> "https://logback.qos.ch/",
        "extref.mysql.base_url" -> "https://www.mysql.com/",
        "extref.oracle.base_url" -> "https://www.oracle.com/database/",
        "extref.play.base_url" -> "https://www.playframework.com/",
        "extref.postgresql.base_url" -> "https://www.postgresql.org/",
        "extref.reactive-manifesto.base_url" -> "https://www.reactivemanifesto.org/",
        "extref.reactive-streams.base_url" -> "https://www.reactive-streams.org/",
        "extref.samplerepo.base_url" -> s"https://github.com/slick/%s",
        "extref.sbt.base_url" -> "https://www.scala-sbt.org/",
        "extref.scala-futures.base_url" -> "https://docs.scala-lang.org/overviews/core/futures.html",
        "extref.scalaquery.base_url" -> "http://scalaquery.org",
        "extref.slf4j.base_url" -> "https://www.slf4j.org/",
        "extref.slick-manuals.base_url" -> "https://scala-slick.org/docs/",
        "extref.slick.base_url" -> s"https://github.com/slick/slick/blob/$ref/%s",
        "extref.sql-server.base_url" -> "https://www.microsoft.com/en-us/sql-server",
        "extref.sqlite.base_url" -> "https://www.sqlite.org/index.html",
        "extref.typesafe-config.base_url" -> "https://github.com/lightbend/config",
        "extref.wikipedia.base_url" -> "https://en.wikipedia.org/wiki/"
      )
    },
    sourceDirectory := baseDirectory.value / "paradox",
    Compile / paradoxTheme / sourceDirectory := baseDirectory.value / "template",
    preprocessDocs / target := target.value / "preprocessed",
    watchSources += sourceDirectory.value,
    watchSources := watchSources.value.filterNot(_.base == (preprocessDocs / target).value),
    preprocessDocs := {
      val out = (preprocessDocs / target).value
      val log = streams.value.log

      IO.copyDirectory(sourceDirectory.value, out)
      IO.copyDirectory(baseDirectory.value / "code", target.value / "code")

      for ((name, dir) <- scaladocDirs.value) {
        val dest = out / name
        log.info(s"Copying $dir to $dest")
        IO.copyDirectory(dir, dest, overwrite = true, preserveLastModified = true)

        (dest ** "*.html").get().foreach { file =>
          modifyFileLines(file) { line =>
            line.replaceAll(
              "(https://github.com/slick/slick/blob/[^\"]*)/" +
                "(Users|home)/" +
                "[^\"]*/slick/target/scala-[^\"]*/src_managed/main/" +
                "([^\"]*)\\.scala",
              """$1/scala/$3.fm"""
            )
          }
        }
      }

      for (sample <- List("hello-slick", "slick-multidb", "slick-testkit-example")) {
        val dir = out / "samples" / sample
        IO.delete(dir)
        ConsoleGitRunner.updated("https://github.com/slick/" + sample, None, dir, log)
        IO.delete(dir / ".git")
      }

      out
    },
    Compile / paradox / unmanagedSourceDirectories := Seq((preprocessDocs / target).value),
    Compile / paradox := (Compile / paradox).dependsOn(preprocessDocs).value,
    Compile / paradox := {
      val outDir = (Compile / paradox).value
      val files = IO.listFiles(outDir, globFilter("*.html"))
      val ref = Versioning.currentRef(baseDirectory.value)
      for (f <- files)
        modifyFileLines(f) { line =>
          line
            .replaceAllLiterally(
              "https://github.com/slick/slick/tree/master/doc/target/preprocessed/",
              s"https://github.com/slick/slick/tree/$ref/doc/paradox/"
            )
            .replaceAllLiterally(
              "https://github.com/slick/slick/tree/master/doc/target/code/",
              s"https://github.com/slick/slick/tree/$ref/doc/code/"
            )
        }
      outDir
    },
    checkScaladocLinks := {
      for ((name, dir) <- scaladocDirs.value)
        new ReusableSbtChecker(dir.toString, (Compile / paradox).value.toString, name, streams.value.log)
          .run()
    },
    addDocsToDocRepo := {
      val dir = (Compile / paradox).value
      addDocsToDocRepoImpl(dir, Versioning.maybeVersionInfo.value, streams.value.log)
    },
    deployDocs := {
      checkScaladocLinks.value

      val log = streams.value.log
      val dir = docRepoCheckoutDir
      val existed = addDocsToDocRepo.value
      log.info("Pushing changes")
      val commitMessage = (if (existed) "Updated" else "Added") + " docs for version " + version.value
      ConsoleGitRunner.commitAndPush(commitMessage)(dir, log)
    },
    showParadoxProperties := {
      val props = (Compile / paradoxProperties).value
      val colWidth = props.keys.map(_.length).max
      for ((k, v) <- props.toSeq.sorted)
        streams.value.log.info(s"%-${colWidth}s    %s".format(k, v))
    }
  )
}

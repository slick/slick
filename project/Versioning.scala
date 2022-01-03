import com.typesafe.sbt.git.DefaultReadableGit
import com.typesafe.tools.mima.plugin.MimaKeys.mimaFindBinaryIssues
import com.typesafe.tools.mima.plugin.MimaPlugin
import coursier.version.Version
import sbt.librarymanagement.CrossVersion
import sbt.{AutoPlugin, Compile, Def, Defaults, Keys, config, inConfig, settingKey, taskKey}
import sbtdynver.DynVerPlugin.autoImport.{dynver, dynverCurrentDate, dynverGitDescribeOutput}
import sbtdynver.{DynVer, GitDescribeOutput}
import sbtversionpolicy.SbtVersionPolicyMima.autoImport.versionPolicyPreviousVersions
import sbtversionpolicy.SbtVersionPolicyPlugin.autoImport._
import sbtversionpolicy.{DependencyCheckReport, Direction, SbtVersionPolicyMima, SbtVersionPolicyPlugin}

import java.io.File
import java.util.Date
import scala.jdk.CollectionConverters._


object Versioning extends AutoPlugin {
  val BumpMinor = Compatibility.BinaryAndSourceCompatible
  val BumpMajor = Compatibility.BinaryCompatible
  val BumpEpoch = Compatibility.None

  def currentRef(dir: File) =
    new DefaultReadableGit(dir)
      .withGit(g => g.currentTags.headOption.getOrElse(g.branch))

  def versionFor(compat: Compatibility, lastTag: String, cleanAfterTag: Boolean): String = {
    if (cleanAfterTag) lastTag
    else {
      val lastVersion = Version(lastTag)
      val nextVersionParts =
        lastVersion.items match {
          case Seq(x: Version.Number, y: Version.Numeric, z: Version.Numeric, t: Version.Tag, _: Version.Numeric)
            if t.isPreRelease                                                 =>
            Seq(x.repr, y.repr, z.repr)
          case Seq(x: Version.Number, y: Version.Numeric, z: Version.Numeric) =>
            compat match {
              case BumpMinor => Seq(x.repr, y.repr, z.next.repr)
              case BumpMajor => Seq(x.repr, y.next.repr, "0")
              case BumpEpoch => Seq(x.next.repr, "0", "0")
            }
          case other                                                          =>
            sys.error(s"Unhandled version format: $other")
        }
      nextVersionParts.mkString(".") + "-SNAPSHOT"
    }
  }

  def versionFor(compat: Compatibility, out: GitDescribeOutput): String = {
    val cleanAfterTag = out.isCleanAfterTag
    val lastTag = out.ref.dropPrefix
    versionFor(compat, lastTag, cleanAfterTag)
  }

  def versionFor(compat: Compatibility, date: Date = new Date): Option[String] =
    DynVer.getGitDescribeOutput(date).map(versionFor(compat, _))

  // Code copied from sbt-version-policy
  private lazy val previousVersionsFromRepo = Def.setting {
    val projId = Keys.projectID.value
    val sv = Keys.scalaVersion.value
    val sbv = Keys.scalaBinaryVersion.value
    val name = CrossVersion(projId.crossVersion, sv, sbv).fold(projId.name)(_ (projId.name))

    val ivyProps = sbtversionpolicy.internal.Resolvers.defaultIvyProperties(Keys.ivyPaths.value.ivyHome)
    val repos = Keys.resolvers.value.flatMap { res =>
      val repoOpt = sbtversionpolicy.internal.Resolvers.repository(res, ivyProps, s => System.err.println(s))
      if (repoOpt.isEmpty)
        System.err.println(s"Warning: ignoring repository ${res.name} to get previous version")
      repoOpt.toSeq
    }
    // Can't reference Keys.fullResolvers, which is a task.
    // So we're using the usual default repositories from coursier here…
    val fullRepos = coursierapi.Repository.defaults().asScala ++ repos
    val res = coursierapi.Versions.create()
      .withRepositories(fullRepos: _*)
      .withModule(coursierapi.Module.of(projId.organization, name))
      .versions()
    res.getMergedListings.getAvailable.asScala
  }


  override def trigger = allRequirements

  val previousRelease = settingKey[Option[String]]("Determine the artifact of the previous release")

  val CompatReport = config("compatReport").extend(Compile)

  val printCompatReport = taskKey[Unit]("Print a compatibility report, e.g., for release notes")

  private def markdownTable(headers: String*)(rows: Seq[Seq[String]]) = {
    val escaped = rows.map(_.map(_.replaceAllLiterally("|", "\\|")))
    val widths =
      (headers.map(_.length) +: escaped.map(_.map(_.length)))
        .transpose
        .map(_.max)

    def row(r: Seq[String]) =
      r
        .zipWithIndex
        .map { case (s, i) => (" " + s + " ").padTo(widths(i) + 2, ' ') }
        .mkString("|", "|", "|\n")

    row(headers) +
      row(widths.map("-" * _)) +
      escaped.map(row).mkString
  }

  override def projectSettings = {
    def fallbackVersion(d: Date): String = s"HEAD-${DynVer.timestamp(d)}"

    List(
      Keys.version :=
        dynverGitDescribeOutput.value.mkVersion(
          versionFor(versionPolicyIntention.value, _),
          fallbackVersion(dynverCurrentDate.value)
        ),
      dynver := {
        val d = new Date
        DynVer.getGitDescribeOutput(d)
          .mkVersion(versionFor(versionPolicyIntention.value, _), fallbackVersion(d))
      },
      previousRelease := {
        val versions = previousVersionsFromRepo.value
        val cur = Version(Keys.version.value)
        val sorted =
          versions.map(Version(_))
            .filterNot { version =>
              version >= cur ||
                version.items.exists { case t: Version.Tag => t.isPreRelease case _ => false }
            }
        if (sorted.isEmpty) None else Some(sorted.max.repr)
      }
    ) ++
      inConfig(CompatReport)(
        Defaults.configSettings ++
          MimaPlugin.projectSettings ++
          SbtVersionPolicyPlugin.projectSettings ++
          SbtVersionPolicyMima.projectSettings ++
          List(
            versionPolicyPreviousVersions := previousRelease.value.toList,
            versionPolicyCheckDirection := Direction.both,
            versionPolicyIntention := BumpMinor,
            printCompatReport := {
              val dependencyIssues = versionPolicyFindDependencyIssues.value.toMap
              val mimaIssues = mimaFindBinaryIssues.value.map { case (module, problems) =>
                module.withName(module.name.stripSuffix("_" + Keys.scalaBinaryVersion.value)) -> problems
              }

              for (module <- (dependencyIssues.keySet ++ mimaIssues.keySet).toSeq.sortBy(_.name)) {
                val depIssues =
                  dependencyIssues.get(module)
                    .map { report =>
                      val binary = report.backwardStatuses.toSet
                      val other = report.forwardStatuses.toSet -- binary
                      binary.map(true -> _).toSeq ++ other.map(false -> _)
                    }
                    .getOrElse(Nil)
                    .filterNot(_._2._2.validated)
                    .sortBy(_._2._1)

                val codeIssues =
                  mimaIssues.get(module)
                    .map { case (backward, forward) =>
                      val binary = backward.toSet
                      val other = forward.toSet -- binary
                      binary.map(true -> _).toSeq ++ other.map(false -> _)
                    }
                    .getOrElse(Nil)

                if (depIssues.nonEmpty || codeIssues.nonEmpty) {
                  println(" ### `" + module + "`")

                  if (depIssues.nonEmpty) {
                    println(" #### Dependency changes")

                    println(
                      markdownTable(
                        "Incompatibility",
                        "Artifact",
                        "Previous version",
                        "Current version",
                        "Version scheme"
                      )(
                        depIssues.map { case (isBinary, ((org, name), status)) =>
                          Seq(if (isBinary) "Binary" else "Source", org + ":" + name) ++
                            (status match {
                              case version: DependencyCheckReport.IncompatibleVersion =>
                                Seq(version.previousVersion, version.version, version.reconciliation.name.capitalize)
                              case missing: DependencyCheckReport.Missing             =>
                                Seq(missing.version, "Absent", "")
                              case _                                                  =>
                                Seq("", "", "")
                            })
                        }
                      )
                    )

                    println()
                  }

                  if (codeIssues.nonEmpty) {
                    println(" #### Code changes")

                    println(
                      markdownTable("Incompatibility", "Problem", "Symbol", "Description")(
                        codeIssues
                          .map { case (isBinary, problem) =>
                            Seq(
                              if (isBinary) "Binary" else "Source",
                              problem.getClass.getSimpleName,
                              problem.matchName.get,
                              problem.description("current")
                            )
                          }
                      )
                    )

                    println()
                  }
                }
              }
            }
          )
      )
  }
}

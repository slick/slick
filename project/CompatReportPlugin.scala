import scala.jdk.CollectionConverters.*

import com.typesafe.tools.mima.core.Problem
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaBinaryIssueFilters, mimaFindBinaryIssues}
import com.typesafe.tools.mima.plugin.MimaPlugin
import coursier.version.Version
import sbt.{config, inConfig, settingKey, taskKey, AutoPlugin, Compile, Def, Defaults, Keys}
import sbt.Keys.projectDependencies
import sbt.librarymanagement.CrossVersion
import sbtversionpolicy.{DependencyCheckReport, Direction, SbtVersionPolicyMima, SbtVersionPolicyPlugin}
import sbtversionpolicy.SbtVersionPolicyMima.autoImport.versionPolicyPreviousVersions
import sbtversionpolicy.SbtVersionPolicyPlugin.autoImport.*


object CompatReportPlugin extends AutoPlugin {
  override def requires = SbtVersionPolicyPlugin

  override def trigger = allRequirements

  val previousRelease = settingKey[Option[String]]("Determine the artifact of the previous release")

  case class ModuleReport(module: String,
                          sinceVersion: String,
                          depChanges: Seq[DependencyChangeInfo],
                          codeChanges: Seq[CodeChangeInfo]) {
    lazy val count = depChanges.length + codeChanges.length
  }

  object autoImport {
    val CompatReport = config("compatReport").extend(Compile)

    val compatReportData =
      taskKey[Seq[ModuleReport]]("Generate the compatibility report data")

    val compatReportMarkdown = taskKey[String]("Generate the compatibility report in Markdown")
  }

  import autoImport.*


  private def markdownTable(headers: String*)(rows: Seq[Seq[String]]) = {
    val escaped = rows.map(_.map(_.replaceAllLiterally("|", "\\|")))
    val widths =
      (headers.map(_.length) +: escaped.map(_.map(_.length)))
        .transpose
        .map(_.max)

    def row(r: Seq[String], pad: Char) =
      r
        .zipWithIndex
        .map { case (s, i) => (pad + s + pad).padTo(widths(i) + 2, ' ') }
        .mkString("|", "|", "|\n")

    row(headers, ' ') +
      row(widths.map("-" * _), '-') +
      escaped.map(row(_, ' ')).mkString
  }

  private def renderDirection(direction: Direction) = {
    (direction.backward, direction.forward) match {
      case (true, true)   => "Backward and forward"
      case (true, false)  => "Backward"
      case (false, true)  => "Forward"
      case (false, false) => "Compatible"
    }
  }

  implicit val directionOrdering: Ordering[Direction] = Ordering.by(d => (d.forward, d.backward))

  case class DependencyChangeInfo(direction: Direction,
                                  organizationId: String,
                                  moduleName: String,
                                  status: DependencyCheckReport.ModuleStatus) {
    val directionString = renderDirection(direction)
    val artifactString = organizationId + ":" + moduleName
    val (previousVersion, currentVersion, versionScheme) = {
      import DependencyCheckReport.{IncompatibleVersion, Missing}
      status match {
        case i: IncompatibleVersion => (i.previousVersion, i.version, i.reconciliation.name.capitalize)
        case m: Missing             => (m.version, "Absent", "")
        case _                      => ("", "", "")
      }
    }
  }

  object DependencyChangeInfo {
    implicit val ordering: Ordering[DependencyChangeInfo] =
      Ordering.by(info => (info.direction, info.organizationId, info.moduleName))
  }

  case class CodeChangeInfo(direction: Direction, problem: Problem) {
    val directionString = renderDirection(direction)
    val symbolName = (problem.matchName: Some[String]).get
    val problemName = problem.getClass.getSimpleName
    val description = problem.description("current")
  }

  object CodeChangeInfo {
    implicit val ordering: Ordering[CodeChangeInfo] =
      Ordering.by(info => (info.direction, info.symbolName, info.problemName))
  }

  private def renderDependencyChangesMarkdownTable(dependencyChanges: Seq[DependencyChangeInfo]) =
    markdownTable("Incompatibility", "Artifact", "Previous version", "Current version", "Version scheme")(
      dependencyChanges.map { info =>
        Seq(
          info.directionString,
          s"`${info.artifactString}`",
          info.previousVersion,
          info.currentVersion,
          info.versionScheme
        )
      }
    )

  private def renderDependencyChangesMarkdownSection(dependencyChanges: Seq[DependencyChangeInfo]) =
    if (dependencyChanges.isEmpty)
      ""
    else
      "#### Dependency changes\n\n" +
        renderDependencyChangesMarkdownTable(dependencyChanges) +
        "\n"

  private def renderCodeChangesMarkdownTable(codeChanges: Seq[CodeChangeInfo]) =
    markdownTable("Incompatibility", "Symbol", "Problem")(
      codeChanges.map { info =>
        Seq(
          info.directionString,
          s"`${info.symbolName}`",
          s"<strong>${info.problemName}</strong><p><small>${info.description}</small></p>"
        )
      }
    )

  private def renderCodeChangesMarkdownSection(codeChanges: Seq[CodeChangeInfo]) =
    if (codeChanges.isEmpty) ""
    else
      "#### Code changes" + "\n\n" +
        renderCodeChangesMarkdownTable(codeChanges)

  private def tupleWithDirection[A](forward: Iterable[A], backward: Iterable[A]) = {
    val b = backward.toSet
    val f = forward.toSet
    val all = b ++ f
    all
      .toSeq
      .map { a =>
        val dir =
          (b.contains(a), f.contains(a)) match {
            case (true, true)   => Direction.both
            case (true, false)  => Direction.backward
            case (false, true)  => Direction.forward
            case (false, false) => Direction.none
          }
        dir -> a
      }
  }

  private def renderModuleMarkdownSection(moduleReport: ModuleReport) =
    moduleReport.count match {
      case 0 => None
      case n =>
        Some(
          s"""
             >### ${moduleReport.module}
             >$n change${if (n == 1) "" else "s"} since ${moduleReport.sinceVersion}
             >
             >${renderDependencyChangesMarkdownSection(moduleReport.depChanges)}
             >${renderCodeChangesMarkdownSection(moduleReport.codeChanges)}
             >""".stripMargin('>')
        )
    }

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
      .withRepositories(fullRepos *)
      .withModule(coursierapi.Module.of(projId.organization, name))
      .versions()
    res.getMergedListings.getAvailable.asScala
  }

  override def projectSettings =
    List(
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
            versionPolicyIntention := Versioning.BumpMinor,
            compatReportData := {
              val projectDeps = projectDependencies.value

              val dependencyIssues =
                versionPolicyFindDependencyIssues.value.toMap
                  .map { case (m, report) => m.toString -> report }

              val filters = mimaBinaryIssueFilters.value

              def stripModuleNameSuffix(name: String) =
                name.stripSuffix("_" + Keys.scalaBinaryVersion.value)

              val mimaIssues =
                mimaFindBinaryIssues.value
                  .map { case (module, (backwardProblems, forwardProblems)) =>
                    module.withName(stripModuleNameSuffix(module.name)).toString ->
                      (backwardProblems.filter(p => filters.forall(f => f(p))),
                        forwardProblems.filter(p => filters.forall(f => f(p))))
                  }

              for (moduleString <- (dependencyIssues.keySet ++ mimaIssues.keySet).toSeq.sorted) yield {
                val depChanges =
                  dependencyIssues.get(moduleString)
                    .fold(Seq.empty[DependencyChangeInfo]) { report =>
                      tupleWithDirection(report.forwardStatuses, report.backwardStatuses)
                        .filter { case (_, ((org, name), status)) =>
                          !status.validated &&
                            !projectDeps.exists(p => p.organization == org && p.name == stripModuleNameSuffix(name))
                        }
                        .map { case (direction, ((org, name), status)) =>
                          DependencyChangeInfo(direction, org, name, status)
                        }
                    }
                    .sorted

                val codeChanges =
                  mimaIssues.get(moduleString)
                    .fold(Seq.empty[CodeChangeInfo]) { case (backward, forward) =>
                      tupleWithDirection(forward, backward)
                        .map { case (direction, problem) => CodeChangeInfo(direction, problem) }
                    }
                    .sorted

                val moduleOrg :+ moduleName :+ sinceVersion = moduleString.split(':').toSeq
                ModuleReport(moduleName, sinceVersion, depChanges, codeChanges)
              }
            },
            compatReportMarkdown := {
              compatReportData.value
                .flatMap(renderModuleMarkdownSection)
                .mkString("\n")
            }
          )
      )
}

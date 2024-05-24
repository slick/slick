import scala.jdk.CollectionConverters.*

import com.typesafe.tools.mima.core.Problem
import com.typesafe.tools.mima.plugin.MimaPlugin
import coursier.version.Version
import sbt.{config, inConfig, settingKey, taskKey, AutoPlugin, Compile, Def, Defaults, Keys}
import sbt.librarymanagement.CrossVersion
import sbtversionpolicy.{DependencyCheckReport, IncompatibilityType, SbtVersionPolicyMima, SbtVersionPolicyPlugin}
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

  private def renderIncompatibilityType(incompatibilityType: IncompatibilityType) =
    incompatibilityType match {
      case IncompatibilityType.BinaryIncompatibility => "Binary"
      case IncompatibilityType.SourceIncompatibility => "Source"
    }

  implicit val incompatibilityTypeOrdering: Ordering[IncompatibilityType] =
    Ordering.by(_ == IncompatibilityType.BinaryIncompatibility)

  case class DependencyChangeInfo(incompatibilityType: IncompatibilityType,
                                  organizationId: String,
                                  moduleName: String,
                                  status: DependencyCheckReport.ModuleStatus)
  object DependencyChangeInfo {
    implicit val ordering: Ordering[DependencyChangeInfo] =
      Ordering.by(info => (info.incompatibilityType, info.organizationId, info.moduleName))
  }

  case class CodeChangeInfo(incompatibilityType: IncompatibilityType, problem: Problem) {
    val directionString = renderIncompatibilityType(incompatibilityType)
    val symbolName = (problem.matchName: Some[String]).get
    val problemName = problem.getClass.getSimpleName
    val description = problem.description("current")
  }

  object CodeChangeInfo {
    implicit val ordering: Ordering[CodeChangeInfo] =
      Ordering.by(info => (info.incompatibilityType, info.symbolName, info.problemName))
  }

  private def renderDependencyChangesMarkdownTable(dependencyChanges: Seq[DependencyChangeInfo]) =
    markdownTable("Artifact", "Incompatibility", "Status", "Previous version", "Current version", "Version scheme")(
      dependencyChanges
        .groupBy(i => (i.organizationId, i.moduleName, i.status))
        .toSeq
        .sortBy(t => (t._1._1, t._1._2))
        .map { case ((organizationId, moduleName, status), infos) =>
          val artifactString = organizationId + ":" + moduleName
          val incompatibilityString = infos.map(i => renderIncompatibilityType(i.incompatibilityType)).mkString(", ")
          s"`$artifactString`" +:
            incompatibilityString +:
            (status match {
              case DependencyCheckReport.IncompatibleVersion(version, previousVersion, reconciliation) =>
                Seq("Incompatible", previousVersion, version, reconciliation.name.capitalize)
              case DependencyCheckReport.Missing(version)                                              =>
                Seq("Missing", version, "", "")
              case _                                                                                   =>
                Seq("", "", "")
            })
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
    val name = CrossVersion(projId.crossVersion, sv, sbv).fold(projId.name)(_(projId.name))

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
            versionPolicyIntention := Versioning.BumpMinor,
            compatReportData := {
              for ((moduleId, (DependencyCheckReport(depReports), codeProblems)) <- versionPolicyFindIssues.value)
                yield
                  ModuleReport(
                    module = moduleId.name,
                    sinceVersion = previousRelease.value.getOrElse("n/a"),
                    depChanges =
                      depReports.toSeq.flatMap { case (incompatibilityType, statuses) =>
                        statuses.collect { case ((org, name), status) if !status.validated =>
                          DependencyChangeInfo(incompatibilityType, org, name, status)
                        }
                      },
                    codeChanges =
                      codeProblems.map { case (incompatibilityType, problem) =>
                        CodeChangeInfo(incompatibilityType, problem)
                      }
                  )
            },
            compatReportMarkdown := {
              compatReportData.value
                .flatMap(renderModuleMarkdownSection)
                .mkString("\n")
            }
          )
      )
}
